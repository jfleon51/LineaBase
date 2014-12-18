----------------------------------------------------------------------------
-- Nombre    : PFFUT512 (Operaciones de futuros)
-- Parametros: Empresa, Portafolio, Rango de fechas
----------------------------------------------------------------------------
  --------------------------------------------------------------------------------------------------
  -- Modificaciones:
  --  CASO   		  	CONSULTOR   	FECHA      DESCRIPCION CAMBIO
  -- 12010401-12080304     EJB	     2012/09/13    Modificación del campo precio en el Informe PFFUT512
  ----------------------------------------------------------------------------

-- Variables del reporte
define _tamrep = 262
define _titrep = 'OPERACIONES DE FUTUROS ENTRE EL &3 Y EL &4'
define _codrep='PFFUT512'


@@id1pflibimp.sql

-- Formatos de columnas

column  especie         heading 'Especie'          format a30
column  contrato        heading 'Contrato'         format a20
column  titulo	        heading 'Titulo'           format 9999999
column  fecha_operacion heading 'Fecha Operación'  format a20
column  tipo_operacion  heading 'Tipo Operación'   format a20
column  cantidad  	    heading 'Cantidad'   	   format 99999
--column  precio        heading 'Precio'	       format 99,999.9999   -- 12010401-12080304
column  precio          heading 'Precio'           format 99,999.999999 -- 12010401-12080304
column  comision        heading 'Comisión'         format 99,999.9999
column  vr_contrato     heading 'Vlr. Contrato'	   format 99999,999,999,999.99
column  vr_pactado     	heading 'Vlr. Pactado'     format 99999,999,999,999.99
column  vr_derecho      heading 'Vlr. Derecho'     format 99999,999,999,999.99
column  estado        	heading 'Estado Actual'    format a20
column  portafolio      heading 'Portafolio'       format a7

-- Rompimientos

break on fecha_operacion skip 1 on report
compute sum of vr_pactado   on fecha_operacion
compute sum of vr_derecho    on fecha_operacion

@@id1pflibexc.sql


Select esp_nombre                                                    	especie
         ,liq_cuenta_futuro                                             contrato
         ,ope_tit_consec						titulo
         ,ope_f_compromiso                                          	fecha_operacion
         ,decode(ope_top_codigo,'1','Compra','2','Venta')   		tipo_operacion
         ,inv_contratos_futuro                                         	cantidad
         ,ope_precio                                                    precio
         ,(select o.ope_valor
             from pfoperaciones_v2 o
            where o.ope_tit_consec = tit_consec
              and o.ope_top_codigo in ('pcomfe','pcomff','pcomfl'))     comision
         ,pcf_monto                                                     vr_contrato
         ,fbd_valor_resultado(tit_emp_codigo
	                     ,inv_cnv_cnv_codigo
	                     ,inv_cnv_codigo
	                     ,tit_vpreal
                             ,inv_precio) * tit_cnv_precio   		vr_pactado
         ,fbd_valor_resultado(ope_emp_codigo
                             ,ope_cnv_cnv_codigo
                             ,ope_cnv_codigo
 	                     ,ope_valor
 	                     ,ope_precio )                              vr_derecho
 	 ,ref_descripcion                  				estado
         ,tit_emp_codigo||'-'||tit_por_codigo                       	portafolio
  from pfinversiones_v2
        ,pftitulos_V2
        ,pfliquidaciones_v2
        ,pfespecies_v2
        ,pfoperaciones_v2
        ,pfcontratos_futuros_v2
        ,pfreferencias_v2
        ,pfview_portafolios_v2
where inv_consec     = tit_inv_consec
   and liq_consec     = nvl(tit_liq_consec_compra,tit_liq_consec_venta)
   and tit_cin_codigo(+) = esp_cin_codigo
   and tit_tin_codigo (+) = esp_tin_codigo
   and tit_emi_codigo (+)= esp_emi_codigo
   and ope_tit_consec = tit_consec
   and pcf_contrato = liq_cuenta_futuro
   and tit_estado = ref_codigo
   and ope_top_codigo in ('1','2')
   and ope_estado <>'OSI'
   and tit_emp_codigo   = view_emp_codigo
   and tit_por_codigo   = view_por_codigo
   and (tit_tit_consec is null )
   and ope_f_compromiso between to_date('&3','yyyymmdd') and to_date('&4','yyyymmdd')
 order by esp_nombre,liq_cuenta_futuro,ope_top_codigo
 ;



exit