/*
Created by: Pablo Rodriguez-Brazzarola
*/


Shiny.addCustomMessageHandler('dm_apply_filter', function(msg){
  console.log(msg)
});


//---------------------------

Shiny.addCustomMessageHandler('vis_group_year_diagnostic', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_year_diagnostic", JSON.stringify(groups));
});


Shiny.addCustomMessageHandler('vis_group_age_diagnostic', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_age_diagnostic", JSON.stringify(groups));
});

Shiny.addCustomMessageHandler('vis_group_histologia', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_histologia", JSON.stringify(groups));
});

Shiny.addCustomMessageHandler('vis_group_estadio', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_estadio", JSON.stringify(groups));
});

Shiny.addCustomMessageHandler('vis_group_localizacion', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_localizacion", JSON.stringify(groups));
});

Shiny.addCustomMessageHandler('vis_group_tratamiento', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_tratamiento", JSON.stringify(groups));
});

Shiny.addCustomMessageHandler('vis_group_intencion', function(msg){
  groups = {};
  for(let id of msg.ids){ groups[id] = $("#"+id).val(); }
  console.log(groups);
  Shiny.onInputChange("current_group_intencion", JSON.stringify(groups));
});