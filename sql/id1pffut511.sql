----------------------------------------------------------------------------
-- Nombre    : PFFUT511 (Posicion en futuros) 
-- Parametros: Empresa, Portafolio                                          
----------------------------------------------------------------------------
-- 14121802 JLG ajuste de mercado

-- Variables del reporte 
define _tamrep = 256
define _titrep = 'POSICION EN FUTUROS'
define _codrep='PFFUT511'

@@id1pflibimp.sql

-- Formatos de columnas 

column  especie         heading 'Especie'          format a30
column  contrato        heading 'Contrato'         format a20
column  titulo          heading 'Titulo'           format 99999999
column  posicion        heading 'Posicion'         format a10
column  cantidad        heading 'Cantidad'         format 99999
column  nominal         heading 'Nominal'	   format 99999,999,999,999.99
column  precio          heading 'Precio'	   format 99,999.9999
column  vr_contrato     heading 'Vlr. Contrato'    format 99999,999,999,999.99
column  vr_mercado      heading 'Vlr. Mercado'     format 99999,999,999,999.99
column  portafolio      heading 'Portafolio'       format a7

-- Rompimientos

break on especie skip 1 on report

compute sum of nominal    on especie
compute sum of vr_mercado on especie

@@id1pflibexc.sql

     
Select esp_nombre                                                    	especie
         ,liq_cuenta_futuro                                             contrato
         ,ope_tit_consec						titulo
         ,decode(ope_top_codigo,'1','Compra','2','Venta')   		posicion
         ,inv_contratos_futuro                                         	cantidad
         ,tit_vr_nominal                                                nominal                  
         ,ope_precio                                                    precio
         ,pcf_monto                                                     vr_contrato
         ,ope_vr_merdao                                                 vr_mercado
         ,tit_emp_codigo||'-'||tit_por_codigo                       	portafolio
  from pfinversiones_v2
        ,pftitulos_V2
        ,pfliquidaciones_v2
        ,pfespecies_v2
        ,pfoperaciones_v2
        ,pfcontratos_futuros_v2
        ,pfview_portafolios_v2
where inv_consec     = tit_inv_consec
   and liq_consec     = nvl(tit_liq_consec_compra,tit_liq_consec_venta)
   and tit_cin_codigo = esp_cin_codigo
   and tit_tin_codigo  = esp_tin_codigo
   and tit_emi_codigo = esp_emi_codigo
   and ope_tit_consec = tit_consec
   and pcf_contrato = liq_cuenta_futuro
   and ope_top_codigo in ('1','2')
   and ope_estado  = 'OPE'
   and ope_f_cancelacion is null
   and tit_emp_codigo   = view_emp_codigo
   and tit_por_codigo   = view_por_codigo
 order by esp_nombre,liq_cuenta_futuro,ope_top_codigo
 ;


exit
