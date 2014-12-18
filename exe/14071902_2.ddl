prompt ____________________________________________________________________________________________
prompt
prompt Archivo....:14071902_2.ddl
prompt Caso.......:14071902
prompt Descripción:Ajuste de estructuras de BD para AXA Acciones
prompt Autor......:JLG
prompt Fecha......:2014/11/25
prompt ____________________________________________________________________________________________


prompt _________________________________________________
prompt
prompt Modificacion de tabla pfcaracteristicas_tit_axa_v2
prompt ________________________________________________

alter table pfcaracteristicas_tit_axa_v2 add cta_por_clase varchar2(3);
alter table pfcaracteristicas_tit_axa_v2 add cta_cant_acciones number;
alter table pfcaracteristicas_tit_axa_v2 add cta_precio_accion number;
alter table pfcaracteristicas_tit_axa_v2 add cta_vlr_conversion number;

comment on column pfcaracteristicas_tit_axa_v2.cta_por_clase is 'Clase de portafolio ZFI,ZVA,ZDE..';
comment on column pfcaracteristicas_tit_axa_v2.cta_cant_acciones is 'Cantidad de acciones negociadas Axa';
comment on column pfcaracteristicas_tit_axa_v2.cta_precio_accion is 'Precio de la acción Axa';
comment on column pfcaracteristicas_tit_axa_v2.cta_por_clase is 'Valor de la tasa de conversion Axa';

prompt ____________________________________________________________________________________________
prompt
prompt Fin de la ejecución 14071902_2.ddl
prompt ____________________________________________________________________________________________
