# Imports
import pickle
import numpy as np
import pandas as pd
import time
# import fasttext

from pathlib import Path
from nltk.stem import SnowballStemmer
from gensim.models.keyedvectors import KeyedVectors

from galennlp_tokens import Tokenizer
from galennlp_fragments.span import SpanS
from galennlp_fragments.fragments.indexed_line import IndexedLine


def remove_accents(word):
    # é - accent acute
    word_aux = word.replace('á', 'a').replace('é', 'e').replace('í', 'i').replace('ó', 'o').replace('ú', 'u')
    # è - accent grave
    word_aux = word_aux.replace('à', 'a').replace('è', 'e').replace('ì', 'i').replace('ò', 'o').replace('ù', 'u')
    # ê - circumflex
    word_aux = word_aux.replace('â', 'a').replace('ê', 'e').replace('î', 'i').replace('ô', 'o').replace('û', 'u')
    # ë - diaerisis
    word_aux = word_aux.replace('ä', 'a').replace('ë', 'e').replace('ï', 'i').replace('ö', 'o').replace('ü', 'u')
    # å - bolle, ã - macron
    word_aux = word_aux.replace('å', 'a').replace('ã', 'a').replace('õ', 'o').replace('ý', 'y')
    # ç - cedilla, č - háček, ø - streg, ð - eth (capital form Ð)
    word_aux = word_aux.replace('ç', 'c').replace('č', 'c').replace('ø', 'o').replace('ð', 'd')

    return word_aux


def remove_symbols(word):
    symbols = ["\x01", '\xad', '#', '@', '$', '€', '&', '·', '_', '*', '\\', '/', '|', '^', "'",
               '‘', '’', '"', '‡', '••', '…', '!', '¡', '¿', '?', '(', ')', '[', ']', '{', '}', '~', '§', '©', 'ª',
               '®', '¯', '¼', '½', '¾', '×', ':', ';', ',', '..', '`']
    word_aux = word

    for s in symbols:
        word_aux = word_aux.replace(s, '')

    return word_aux


class NeoplasmClassification:

    def __init__(self, datadir, classifier, embedding):

        self._root_ = Path(datadir)

        # Check if  datadir exist
        assert self._root_.exists(), self._root_.resolve().absolute()

        # Prepare Tokenizer
        self.tokenizer = Tokenizer()

        # Punctuation to remove based on string.punctuation
        # +-*<=>~% will not be removed because they may be clinically important
        self.punctuation = ['!', '¡', '¿', '?', '"', "'", '`', '^', '(', ')', '[', ']', '{', '}', '.', ',', ':',
                            ';', '_', '\\', '/', '|', '@', '#', '$', '&']

        # Dictionary for decode labels
        self.dic_decoder = {0: 'Mama', 1: 'Pulmon', 2: 'Colorectal', 3: 'Otro'}

        # Get the stemmer from nltk
        self.stemmer = SnowballStemmer('spanish')

        # Possible classifier to use
        classifier_options = ["RNN", "ML", "fastText"]
        # Get the desired classifier to predict neoplasm.
        if classifier not in classifier_options:
            exit("ERROR: not such classifier as " + str(classifier))
        else:
            self.classifier = classifier
            self.classifier_dir = f"{datadir}models/"
            print("model 1")
            self.classifier_model = self.load_model()
            print("model 2")
            

        # If classifier is "RNN" or "ML", we need to load BoW or embedding
        if self.classifier in "RNN|ML" and embedding is not None:
            self.embedding_dir = f"{datadir}embeddings/"
            print("Emb 1")
            self.embedding_model = self.load_embedding()
            print("Emb 2")

            if self.classifier in "RNN":
                self.idx_unknown = len(self.embedding_model.wv.key_to_index.keys()) + 1
        elif self.classifier in "RNN":
            exit("ERROR: an embedding model must be specified.")

    def process_text(self, documents_from_R):
        """Tokenize the document and get the desired format (RNN and ML)."""
        # Convert from dict(Object from R) to numpy (Desired object)
        documents_from_R = documents_from_R.to_numpy()

        # Initialization to avoid reference warnings.
        document_lines, docs, file_HCES,  = [
            [], [], []]

        # Each row is a document with a list of lines
        for document in documents_from_R:
            # Get the correct format depend on the classifier used
            if self.classifier in "RNN":
                """Each row is a document composed by all the tokens"""
                document_lines = []
            else:
                """Each row is a document in string format"""
                document_lines = ""

            # Each document.
            ind_line = IndexedLine.from_document_chunk(document[1], 0, SpanS.create_from_end(0, len(document[1])))

            # Parse line and get each token.
            for token in self.tokenizer.parse(ind_line):
                if not token.token.value in self.punctuation:
                    # If tag is different to None, token was detected by tokenizer
                    if token.token.tag is None:
                        stem_token = remove_accents(token.token.value)
                        stem_token = remove_symbols(stem_token)
                        # It isn't an empty word
                        if len(stem_token) > 0:
                            stem_token = self.stemmer.stem(stem_token)
                    else:
                        # This token doesn't need to be stemmed
                        stem_token = token.token.value

                    # It isn't an empty word
                    if len(stem_token) > 0:
                        if self.classifier in "RNN":
                            document_lines.append(self.transform_word2index(stem_token))
                        else:
                            document_lines = document_lines + " " + stem_token

            # Save document
            docs.append(document_lines)

            # Save File dni
            file_HCES.append(document[0])

           

        if self.classifier in "RNN":
            # Save tokenized documents with numpy format again
            documents_numpy = np.array(docs, dtype=object)
            return file_HCES, documents_numpy
        else:
            # Save tokenized documents with numpy format again
            documents_numpy = np.array(docs, dtype=object)

            if self.classifier in "ML":
                # Transform data with BoW model.
                documents_numpy = self.transform_bow(documents_numpy)

            return file_HCES, documents_numpy

    def transform_word2index(self, word):
        return self.embedding_model.wv.get_index(word, self.idx_unknown) + 1

    def transform_bow(self, data):
        return self.embedding_model.transform(data)

    def load_model(self):
        if self.classifier in "ML":
            # Load machine learning model
            with open((self.classifier_dir + 'naive_model.pkl').replace("/","\\"), 'rb') as f:
                clf_model = pickle.load(f)
        # elif self.classifier in "fastText":
            
        #     # Load fastText model
        #     clf_model = fasttext.load_model((self.classifier_dir + 'fastText.bin').replace("/","\\"))
            
        else:
            # Por el momento vacio
            clf_model = None
        return clf_model

    def load_embedding(self):
        if self.classifier in "ML":
            # Load BoW tf-idf model
            with open((self.embedding_dir + 'tfidf_vect.pkl').replace("/","\\"), 'rb') as f:
                emb_model = pickle.load(f)
        else:
            # Load Word2Vec or fastText model
            emb_model = KeyedVectors.load(self.embedding_dir.replace("/","\\"), mmap="r")
        return emb_model

    def predict_fasttext(self, x_test):
        dic_label = {"__label__Mama": 0, "__label__Pulmon": 1, "__label__ColonRecto": 2, "__label__Otro": 3}

        predicts = self.classifier_model.predict_proba(x_test, 4)
        predict_numpy = np.zeros(shape=[len(predicts), 4])

        for i, i_tuple in enumerate(predicts):
            for j, pred in enumerate(i_tuple):
                lab_assoc = dic_label[pred[0]]
                predict_numpy[i, lab_assoc] = pred[1]

        return predict_numpy

    def run(self, documents_from_R):

       

        # Tokenize and prepare documents.
        files_HCES, docs_numpy = self.process_text(documents_from_R)

        # File HCEs
        files_HCES = np.array(files_HCES, dtype=object)

        # Get Predictions
        if self.classifier in "fastText":
            predictions_proba = self.predict_fasttext(docs_numpy)
        else:
            predictions_proba = self.classifier_model.predict_proba(docs_numpy)

        # Get Labels
        predictions = [self.dic_decoder[int(np.argmax(proba))] for proba in predictions_proba]

        codes = [np.argmax(proba) for proba in predictions_proba]

        tabular_data = np.column_stack((files_HCES,predictions,codes, predictions_proba))

        # Crear un DataFrame de Pandas a partir de la matriz y asignar nombres de columna

        df = pd.DataFrame(tabular_data, columns=['ID_Paciente', 'Neoplasia_Predicha',"Neoplasia_Code", 'Mama', 'Pulmon', 'Colorectal', 'Otro'])

        # Devolver el df
        return(df)

