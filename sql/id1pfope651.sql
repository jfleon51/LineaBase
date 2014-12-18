----------------------------------------------------------------------------
-- Nombre    : PFOPE651 (Informe Operaciones Sin Cumplimiento).
-- Parametros: Empresa, Portafolio, Fecha
----------------------------------------------------------------------------
-- Variables del reporte 
define _tamrep = 331
define _titrep = 'INFORME DE OPERACIONES - FOGAFIN - AL &3'
define _codrep = 'PFOPE651'

@@id1pflibimp.sql

-- Formatos de columnas 
column portafolio		heading 'Portafolio' 		format a25
column tipo_operacion		heading 'Tipo de Operación'	format a50
column titulo			heading 'N° Título'		format 99999999
column emisor			heading 'Emisor'		format a50
column isin			heading 'ISIN'			format a20
column moneda_origen 		heading 'Moneda Origen'		format 99,999,999.99
column fecha_emision		heading 'Fecha Emisión'		format a20
column fecha_vencimiento	heading 'Fecha de Vencimiento'  format a20
column valor_nominal		heading 'Valor Nominal'		format 999,999,999,999.99
column fecha_negocio		heading 'Fecha de negocio'	format a20
column fecha_cumplimiento	heading 'Fecha de Cumplimiento' format a21
column fecha_liquidacion	heading 'Fecha de Liquidación'	format a20
column valor_mercado		heading 'Valor de Mercado'	format 999,999,999,999.99
column valor_giro		heading 'Valor de Giro'		format 999,999,999,999.99

@@id1pflibexc.sql

-- Query
select distinct por_descripcion                                                     portafolio,
       top_descripcion                                                     tipo_operacion,
       ope_tit_consec                                                      titulo,
       emi_nombre                                                          emisor,
       fbd_isin(ope_tit_consec)                                            isin,
       ope_cnv_codigo                                                      moneda_origen,
       to_char(inv_f_emision, 'yyyy/mm/dd')                                fecha_emision,
       to_char(inv_f_vcto, 'yyyy/mm/dd')                                   fecha_vencimiento,
       fbd_nominal(ope_tit_consec, to_date('&3', 'yyyymmdd'))              valor_nominal,
       to_char(decode(fbd_ope_contado(ope_tit_consec, ope_top_codigo),
                                         'S' ,ope_f_compromiso
                                             ,null),'yyyy/mm/dd')          fecha_negocio,
       to_char(ope_fecha, 'yyyy/mm/dd')                                    fecha_cumplimiento,
       to_char(ope_f_habil, 'yyyy/mm/dd')                                  fecha_liquidacion,
--       decode(fbd_ope_contado(ope_tit_consec, ope_top_codigo),
--                                         'S' ,tit_vppm
--                                             ,null)                        valor_mercado
       valor_mercado
      /* , decode(ope_cnv_pago
                     ,null                             ,(ope_vr_neto - case when (to_date('&3', 'yyyymmdd') <= ope_f_cumplimiento) then nvl(ope_vr_recibido,0) else 0 end) * 
                                                         nvl(ope_precio,1)*nvl(ope_cnv_precio,1)
                     ,fbd_moneda_emp(ope_emp_codigo)   ,ope_valor-case when (to_date('&3', 'yyyymmdd') <= ope_f_cumplimiento) then nvl(ope_vr_recibido,0) else 0 end
                                                       ,(ope_vr_neto - case when (to_date('&3', 'yyyymmdd') <= ope_f_cumplimiento) then nvl(ope_vr_recibido,0) else 0 end) * 
                                                         nvl(ope_precio,1)*nvl(ope_cnv_precio,1)
               )*/
     , ope_valor valor_giro                                                          
  from ( -- Operaciones de contado
        select ope_tit_consec,
               ope_cln_codigo,
               ope_f_compromiso,
               ope_fecha,
               fbd_habil(ope_fecha) ope_f_habil,
               decode(ope_top_codigo
                    ,'1',vti_car_vpngc
                        ,vti_car_vpng
                    )                valor_mercado,
               ope_top_codigo,
               ope_cnv_codigo,
               ope_cnv_cnv_codigo,
               ope_valor,
               ope_emp_codigo,
               ope_por_codigo,
               ope_vr_recibido,
               ope_f_cumplimiento,
               ope_cnv_precio,
               ope_precio,
               ope_vr_neto,
               ope_cnv_pago
          from pfvaloracion_titulos_v2
              ,pfportafolios_v2
              ,pfoperaciones_v2
              ,pfview_portafolios_v2
        where to_date('&3','yyyymmdd') = decode(nvl(por_fondo,'N')
                                         ,'N',decode(ope_top_codigo,'1',vti_f_resumen_carc,vti_f_resumen_carv)
                                         ,'S',vti_fecha_cierre
                                         )
          and vti_tit_consec           = ope_tit_consec
          and por_codigo               = ope_por_codigo
          and por_emp_codigo           = ope_emp_codigo
          and fbd_ope_contado(ope_tit_consec, ope_top_codigo) = 'S'
          and ope_top_codigo in ('1', '2')
          and ope_f_compromiso        <= to_date('&3', 'yyyymmdd')
          and ope_fecha               >= to_date('&3', 'yyyymmdd')
          and fbd_incluye(ope_consec) = 'S'
          and ((ope_flujo_real = 'S') or
               (ope_flujo_real = 'N' and ope_estado = 'ORF'))
          and view_emp_codigo = ope_emp_codigo
          and view_por_codigo = ope_por_codigo
        union all
        -- Pago de cupón o Reintegro de capital
        select ope_tit_consec,
               ope_cln_codigo,
               ope_f_compromiso,
               ope_fecha,
               fbd_habil(ope_fecha) ope_f_habil,
               null                 valor_mercado,
               ope_top_codigo,
               ope_cnv_codigo,
               ope_cnv_cnv_codigo,
               ope_valor,
               ope_emp_codigo,
               ope_por_codigo,
               ope_vr_recibido,
               ope_f_cumplimiento,
                ope_cnv_precio,
                ope_precio,
                ope_vr_neto,
                ope_cnv_pago
          from pfoperaciones_v2, pfview_portafolios_v2
         where ope_top_codigo = '4'
           and ope_fecha between to_date('&3', 'yyyymmdd') and to_date('&3', 'yyyymmdd') + 14
           and fbd_incluye(ope_consec) = 'S'
           and ope_flujo_real   = 'S'
           and view_emp_codigo  = ope_emp_codigo
           and view_por_codigo  = ope_por_codigo
        union all
        -- Vencimientos de operaciones diferentes a contado
        select ope_tit_consec,
               ope_cln_codigo,
               ope_f_compromiso,
               ope_fecha,
               fbd_habil(ope_fecha) ope_f_habil,
               null                 valor_mercado,
               ope_top_codigo,
               ope_cnv_codigo,
               ope_cnv_cnv_codigo,
               ope_valor,
               ope_emp_codigo,
               ope_por_codigo,
               ope_vr_recibido,
               ope_f_cumplimiento,
               ope_cnv_precio,
               ope_precio,
               ope_vr_neto,
               ope_cnv_pago
          from pfoperaciones_v2, pfview_portafolios_v2
         where fbd_ope_contado(ope_tit_consec, ope_top_codigo) = 'N'
           and ope_top_codigo in ('1', '2', '3')
           and ope_fecha between to_date('&3', 'yyyymmdd') and to_date('&3', 'yyyymmdd') + 14
           and fbd_incluye(ope_consec) = 'S'
           and ope_flujo_real = 'S'
           and view_emp_codigo = ope_emp_codigo
           and view_por_codigo = ope_por_codigo
       ) vope,
       pfportafolios_v2,
       pftipo_operaciones_v2,
       pftitulos_v2,
       pfemisores_v2,
       pfinversiones_v2
 where por_clase = 'ZFI'
   and vope.ope_emp_codigo = por_emp_codigo
   and vope.ope_por_codigo = por_codigo
   and vope.ope_top_codigo = top_codigo
   and vope.ope_tit_consec = tit_consec
   and inv_consec = tit_inv_consec
   and emi_codigo = inv_emi_codigo
 order by  
  
/

exit
