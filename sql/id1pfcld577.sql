----------------------------------------------------------------------------
-- Nombre    : PFCLD577 (Valoracion Futuros a una fecha - Divisas) 
-- Parametros: Empresa, Portafolio, Fecha                                          
----------------------------------------------------------------------------
-- Variables del reporte 
define _tamrep = 280 --195
define _titrep = 'VALORACION FUTUROS Al &3'
define _codrep = 'PFCLD577'

drop function fn_anterior;
drop function fn_fecha_ant;


@@id1pflibimp.sql

-- Formatos de columnas 

column  contrato        heading 'Contrato'         format a20
column  Titulo          heading 'Titulo'           format 999999
column  vcto            heading 'Vencimiento'      format a10
column  compra_venta    heading 'Compra/Venta'     format a1
column  contraparte     heading 'Contraparte'      format a30
column  pmercado        heading 'Precio Mercado'   format 99,999.999999
column  vlr_dolar       heading 'Vlr. USD'         format 99999,999,999.99
column  cantidad        heading 'Nro.Contratos'    format 99999
column  total_dolar     heading 'Monto USD'        format 99999,999,999.99
column  monto_neg       heading 'Monto Neg.'       format 99999,999,999,999.99
column  vlr_merc_ant    heading 'Vlr.Mercado Ant.' format 99999,999,999,999.99
column  vlr_mercado     heading 'Valor Mercado'    format 99999,999,999,999.99
column  utilidad        heading 'Util./Perd.Acum'  format 99999,999,999,999.99
column  utilidad_dia    heading 'Util./Perd.Dia'   format 99999,999,999,999.99
column  port            heading 'Por'              format a7
column  objetivo        heading 'Objetivo'         format a11

-- Rompimientos

break on tipo_operacion skip 1 on operacion on report

compute sum of utilidad on report
compute sum of utilidad_dia on report

@@id1pflibexc.sql

-- Funcion que retorna la ultima fecha de cierre del titulo anterior al parametro.

create or replace 
function fn_fecha_ant(pff_titulo number) return date is
  t_fecha date;

begin
  select max(vti_fecha_cierre)
    into t_fecha
    from pfvaloracion_titulos_v2
   where vti_fecha_cierre < to_date('&3','yyyymmdd')
     and vti_tipo_cierre = 'RR'
     and vti_tit_consec  = pff_titulo
  ;
  return (t_fecha);
end;
/

-- Funcion que calcula en Valor de Causacion a la fecha anterior a la del informe 

create or replace 
function fn_anterior(pfa_titulo number,pfa_fecha date,pfa_cual varchar2) return number is
  t_valor number;
begin
  select decode(pfa_cual,'C',nvl(vti_causacion,0)
                        ,'M',fbd_valor_resultado(tit_emp_codigo
                                                ,inv_cnv_cnv_codigo
                                                ,inv_cnv_codigo
                                                ,vti_vppm
                                                ,vti_preciom)
               )                                 
    into t_valor
    from pfinversiones_v2
        ,pftitulos_v2
        ,pfvaloracion_titulos_v2
   where inv_consec       = tit_inv_consec
     and tit_consec       = vti_tit_consec
     and vti_fecha_cierre = pfa_fecha
     and vti_tit_consec   = pfa_titulo
     and vti_tipo_cierre  = 'RR'
  ;
  return (t_valor);
end;
/

     
-- Vigentes
 select liq_cuenta_futuro                                              contrato
       ,tit_consec						       titulo
       ,to_char(inv_f_vcto,'yyyy/mm/dd')                               vcto
       ,decode(ope_top_codigo
              ,'1','C'
                  ,'V'
              )                                                        compra_venta
       ,fbd_contraparte(ope_consec)                                    contraparte
       ,min(vti_preciom)                                               pmercado
       ,min(decode(inv_cnv_codigo
              ,fbd_emp_unidad(tit_emp_codigo),inv_vr_nominal
                                             ,fbd_valor_resultado(tit_emp_codigo
                                                                 ,inv_cnv_cnv_codigo
                                                                 ,inv_cnv_codigo
                                                                 ,inv_vr_nominal
                                                                 ,inv_precio
                                                                 )
              ))                                                       vlr_dolar
       ,nvl(inv_contratos_futuro,count(*))                             cantidad
       ,sum(decode(inv_cnv_codigo
                  ,fbd_emp_unidad(tit_emp_codigo),inv_vr_nominal
                                                 ,fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     )
                  ))                                                   total_dolar
       ,sum(decode(inv_cnv_codigo
                  ,fbd_emp_unidad(tit_emp_codigo),fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     ) 
                                                 ,fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     ) * inv_cnv_precio
                  )
           )                                                           monto_neg       
       ,sum(fn_anterior(vti_tit_consec
                       ,fn_fecha_ant(vti_tit_consec)
                       ,'M'
                       ))                                              vlr_merc_ant
       ,sum(fbd_valor_resultado(tit_emp_codigo
                               ,inv_cnv_cnv_codigo
                               ,inv_cnv_codigo
                               ,vti_vppm
                               ,vti_preciom
                               ))                                      vlr_mercado
       ,sum((fbd_valor_resultado(tit_emp_codigo
                                ,inv_cnv_cnv_codigo
                                ,inv_cnv_codigo
                                ,vti_vppm
                                ,vti_preciom
                                )-
             fbd_valor_resultado(tit_emp_codigo
                                ,inv_cnv_cnv_codigo
                                ,inv_cnv_codigo
                                ,inv_vr_nominal
                                ,tit_car_vpnm)) *
             decode(inv_operacion,'1',1,-1)                   
           )                                                           utilidad
       ,sum(vti_causacion)                                             utilidad_dia
       ,' '||min(vti_emp_codigo||'-'||vti_por_codigo)                  port
       ,' '||min(fbd_objetivo_operacion(tit_consec,'&3'))              objetivo
   from pftipos_operacion_divisas_v2
       ,pfinversiones_v2
       ,pftitulos_v2
       ,pfoperaciones_v2
       ,pfvaloracion_titulos_v2
       ,pfportafolios_v2
       ,pfliquidaciones_v2
       ,pfview_portafolios_v2
  where nvl(tod_tipo_otros,'X') = 'F'
  --  and nvl(tod_subyacente,'S') = 'N'
    and tod_plazo        is null
    and tod_codigo       = inv_tipo_operacion
    and inv_consec       = tit_inv_consec
    and liq_consec       = nvl(tit_liq_consec_compra,tit_liq_consec_venta)
    and fbd_vigente(vti_tit_consec,to_date('&3','yyyymmdd')) = 'S'
    and tit_estado      <> 'TSI'        
    and tit_consec       = ope_tit_consec    
    and ope_ope_consec     is null
    and ((ope_top_codigo   = '1') or
         (ope_top_codigo   = '2')
         )
    and tit_consec       = vti_tit_consec
    and vti_f_resumen    >= fbd_nace(vti_tit_consec)
    and vti_tipo_cierre  = 'RR'
    and vti_fecha_cierre = to_date('&3','yyyymmdd')
    and vti_por_codigo   = por_codigo
    and vti_emp_codigo   = por_emp_codigo
    and por_emp_codigo   = view_emp_codigo
    and por_codigo       = view_por_codigo
group by tit_consec
        ,liq_cuenta_futuro
        ,inv_f_vcto
        ,inv_operacion
        ,ope_top_codigo
        ,fbd_contraparte(ope_consec) 
        ,inv_contratos_futuro
/

 select liq_cuenta_futuro                                              contrato
       ,tit_consec						       titulo
       ,to_char(inv_f_vcto,'yyyy/mm/dd')                               vcto
       ,decode(ope_top_codigo
              ,'1','C'
                  ,'V'
              )                                                        compra_venta
       ,fbd_contraparte(ope_consec)                                    contraparte       
       ,min(vti_preciom)                                               pmercado
       ,min(decode(inv_cnv_codigo
              ,fbd_emp_unidad(tit_emp_codigo),inv_vr_nominal
                                             ,fbd_valor_resultado(tit_emp_codigo
                                                                 ,inv_cnv_cnv_codigo
                                                                 ,inv_cnv_codigo
                                                                 ,inv_vr_nominal
                                                                 ,inv_precio
                                                                 )
              ))                                                       vlr_dolar
       ,nvl(inv_contratos_futuro,count(*))                             cantidad
       ,sum(decode(inv_cnv_codigo
                  ,fbd_emp_unidad(tit_emp_codigo),inv_vr_nominal
                                                 ,fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     )
                  ))                                                   total_dolar
       ,sum(decode(inv_cnv_codigo
                  ,fbd_emp_unidad(tit_emp_codigo),fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     ) 
                                                 ,fbd_valor_resultado(tit_emp_codigo
                                                                     ,inv_cnv_cnv_codigo
                                                                     ,inv_cnv_codigo
                                                                     ,inv_vr_nominal
                                                                     ,inv_precio
                                                                     ) * inv_cnv_precio
                  )
           )                                                           monto_neg       
       ,sum(fn_anterior(vti_tit_consec
                       ,fn_fecha_ant(vti_tit_consec)
                       ,'M'
                       ))                                              vlr_merc_ant
       ,sum(fbd_valor_resultado(tit_emp_codigo
                               ,inv_cnv_cnv_codigo
                               ,inv_cnv_codigo
                               ,vti_vppm
                               ,vti_preciom
                               ))                                      vlr_mercado
       ,sum((fbd_valor_resultado(tit_emp_codigo
                               ,inv_cnv_cnv_codigo
                               ,inv_cnv_codigo
                               ,vti_vppm
                               ,vti_preciom
                               )-
            fbd_valor_resultado(tit_emp_codigo
                               ,inv_cnv_cnv_codigo
                               ,inv_cnv_codigo
                               ,inv_vr_nominal
                               ,tit_car_vpnm)) *
             decode(inv_operacion,'1',1,-1)    
           )                                                           utilidad
       ,sum(vti_causacion)                                             utilidad_dia
       ,' '||min(vti_emp_codigo||'-'||vti_por_codigo)                  port
       ,' '||min(fbd_objetivo_operacion(tit_consec,'&3'))              objetivo
   from pftipos_operacion_divisas_v2
       ,pfinversiones_v2
       ,pftitulos_v2
       ,pfoperaciones_v2       
       ,pfvaloracion_titulos_v2
       ,pfportafolios_v2
       ,pfliquidaciones_v2
       ,pfview_portafolios_v2
  where nvl(tod_tipo_otros,'X') = 'F'
 --   and nvl(tod_subyacente,'S') = 'N'
    and tod_plazo        is null
    and tod_codigo       = inv_tipo_operacion
    and inv_consec       = tit_inv_consec
    and liq_consec       = nvl(tit_liq_consec_compra,tit_liq_consec_venta)
    and fbd_vigente(vti_tit_consec,to_date('&3','yyyymmdd')) = 'N'
    and tit_estado      <> 'TSI'
    and tit_consec       = ope_tit_consec    
    and ope_ope_consec     is null
    and ((ope_top_codigo   = '1') or
         (ope_top_codigo   = '2')
        )
    and tit_consec       = vti_tit_consec
    and vti_f_resumen    >= fbd_nace(vti_tit_consec)
    and vti_tipo_cierre  = 'RR'
    and vti_fecha_cierre = to_date('&3','yyyymmdd')
    and vti_por_codigo   = por_codigo
    and vti_emp_codigo   = por_emp_codigo
    and por_emp_codigo   = view_emp_codigo
    and por_codigo       = view_por_codigo
group by tit_consec
        ,liq_cuenta_futuro
        ,inv_f_vcto
        ,inv_operacion
        ,ope_top_codigo
        ,fbd_contraparte(ope_consec) 
        ,inv_contratos_futuro
/

drop function fn_anterior;

drop function fn_fecha_ant;

exit
