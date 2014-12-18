set termout off
set lines 1000
set pages 0
column Producto     heading 'Producto'     format a15
column Patch        heading 'Patch'        format a16
column ajuste_patch heading 'Ajuste Patch' format a24

spool parametros_cliente.log

prompt *****************************************************************************************
select '* Parametrización Cliente: '||ref_descripcion ||' ('||ref_codigo||')'
  from pfreferencias_v2
 where ref_codigo = fbd_cliente
/
select '* Instancia de BD: '||global_name||', Conectado con usuario: '||user||
       ' Fecha de Generación: '||to_char(sysdate,'yyyy/mm/dd hh24:mi:ss') fecha_hora
  from global_name
/
set pages 100
Prompt * Información del Patch al que está el Cliente según el aplicativo

select  '* '||prd_codigo                          Producto
       ,max(prd_patch_actual)                     Patch
       ,min(nvl(prd_ajuste_actual,'[NINGUNO]'))   Ajuste_Patch
  from  agproductos 
       ,agaplicaciones 
 where prd_codigo = apl_prd_codigo 
 group by prd_codigo
/

Prompt * Objetos Inválidos a la fecha de generación del éste informe.
select object_type                                          tipo
      ,object_name                                          nombre
      ,status                                               estado
  from obj o
 where to_number(object_id) > nvl(to_number(data_object_id),0)
   and status = 'INVALID'
   and object_type in ('PROCEDURE','FUNCTION','VIEW','PACKAGE','PACKAGE BODY','TRIGGER')
 order by 1,2
/

Prompt * Información sobre la plataforma usada en el cliente (v$database)
select name, db_unique_name, platform_name
  from v$database
/

Prompt * Información sobre la versión de BD utilizada por el cliente (v$instance)
select instance_name, host_name, version, status
  from v$instance
/  

set pages 0
prompt *****************************************************************************************

prompt
prompt
prompt -- Descripción de la tabla de empresas (pfempresas_v2)
prompt
describe pfempresas_v2
prompt
prompt -- Descripción de la tabla de portafolios (pfportafolios_v2)
prompt
describe pfportafolios_v2
prompt
prompt -- Descripción de la tabla de especies (pfespecies_v2)
prompt
describe pfespecies_v2
prompt
prompt -- Descripción de la tabla de series (pfseries_v2)
prompt
describe pfseries_v2

prompt
prompt -- Descripción del paquete pkg_exp
prompt
describe pkg_exp

set pages 1000

prompt
Prompt -- Información de la tabla de referencias.
prompt
select *
  from pfreferencias_v2
/

prompt
Prompt -- Información de la tabla de conceptos contables.
prompt
select *
  from pfconceptos_contables_v2
/

prompt
Prompt -- Información de la tabla de tipo deoperaciones.
prompt
select *
  from pftipo_operaciones_v2
/


set pages 0
prompt
prompt
prompt
prompt ****** INFORMACIÓN A SINCRONIZAR CON LÍNEA BASE ***********
prompt
prompt
prompt
prompt -- Insertando los Reportes (agreportes) registrados del cliente.
prompt

select 'insert into agreportes (rep_aplicacion,rep_mod_codigo,rep_descripcion,rep_ejecutable,rep_estado,rep_tipo,rep_bloque) '||
       'values('''||rep_aplicacion||''''||','||''''||rep_mod_codigo||''''||','||''''||rep_descripcion||''''||','||''''||rep_ejecutable||''''||','||''''||
       rep_estado||''''||','||''''||rep_tipo||''''||','||''''||rep_bloque||''');' sentencia
  from agreportes
/

prompt
prompt -- Insertando los mólulos (agmodulos) registrados del cliente.
prompt

select 'insert into agmodulos(mod_aplicacion, mod_codigo, mod_mod_codigo, mod_descripcion, mod_tipo, mod_ejecutable, mod_estado, mod_orden) '||
       'values('''||mod_aplicacion||''''||','||''''||mod_codigo||''''||','||''''||mod_mod_codigo||''''||','||''''||mod_descripcion||''''||','||''''||
       mod_tipo||''''||','||''''||mod_ejecutable||''''||','||''''||mod_estado||''''||','||''''||mod_orden||''');' sentencia
  from agmodulos

prompt
prompt -- Insertando las Referencias de Seguridad (agreferencias) registadas en el cliente 
prompt

select 'insert into agreferencias(sre_codigo, sre_descripcion, sre_significado, sre_significado2) '||
       'values('''||sre_codigo||''''||','||''''||sre_descripcion||''''||','||''''||sre_significado||''''||','||''''||sre_significado2||''');' sentencia
  from agreferencias

prompt
prompt -- Insertando las Referencias de Porfin (pfreferencias_v2) registadas en el cliente 
prompt

select 'insert into pfreferencias_v2(ref_codigo, ref_descripcion, ref_valor) '||
       'values('''||ref_codigo||''''||','||''''||ref_descripcion||''''||','||''''||ref_valor||''');'
  from pfreferencias_v2  
/

prompt
prompt -- Insertando los conceptos contables (pfconceptos_contables_v2) registados en el cliente 
prompt

select 'insert into pfconceptos_contables_v2 (cco_codigo,cco_tipo_movto,cco_nombre,cco_formula,cco_descripcion,cco_contexto,cco_qud_codigo) '||
       'values('''||cco_codigo||''''||','||''''||cco_tipo_movto||''''||','||''''||cco_nombre||''''||','||''''||cco_formula||''''||','||''''||
       cco_descripcion||''''||','||''''||cco_contexto||''''||','||''''||cco_qud_codigo||''');' sentencia
  from pfconceptos_contables_v2
/

prompt
prompt -- Insertando los tipos de operaciones (pftipo_operaciones_v2) registrados en el cliente
prompt

select 'insert into pftipo_operaciones_v2 (top_codigo,top_descripcion,top_signo,top_calculo_contable,top_var_nominal,top_top_codigo,top_desc_pasivo,top_aplica_valoracion,top_indicador_movimiento,top_negociabilidad,top_tipo_com,top_ref_tipo_com,top_rubro,top_contabilizar,top_cuenta,top_cuenta_aux,top_ref_tipo_doc,top_requiere_nit,top_estado,top_max_operaciones,top_aplica_sobre,top_retiro_minimo,top_cuenta_aux2,top_calcula_nominal,top_modificable,top_tipo,top_copia,top_cnv_codigo) '||
       'values('''||top_codigo||''''||','||''''||top_descripcion||''''||','||''''||top_signo||''''||','||''''||top_calculo_contable||''''||','||''''||
       top_var_nominal||''''||','||''''||top_top_codigo||''''||','||''''||top_desc_pasivo||''''||','||''''||top_aplica_valoracion||''''||','||''''||
       top_indicador_movimiento||''''||','||''''||top_negociabilidad||''''||','||''''||top_tipo_com||''''||','||''''||top_ref_tipo_com||''''||','||''''||
       top_rubro||''''||','||''''||top_contabilizar||''''||','||''''||top_cuenta||''''||','||''''||top_cuenta_aux||''''||','||''''||
       top_ref_tipo_doc||''''||','||''''||top_requiere_nit||''''||','||''''||top_estado||''''||','||''''||top_max_operaciones||''''||','||''''||
       top_aplica_sobre||''''||','||''''||top_retiro_minimo||''''||','||''''||top_cuenta_aux2||''''||','||''''||top_calcula_nominal||''''||','||''''||
       top_modificable||''''||','||''''||top_tipo||''''||','||''''||top_copia||''''||','||''''||top_cnv_codigo||''');' sentencia
  from pftipo_operaciones_v2
/

spool off
set termout on
