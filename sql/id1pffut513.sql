----------------------------------------------------------------------------
-- Nombre    : PFFUT513 (Informe Utilidad Realizada) 
-- Parametros: Empresa, Portafolio, Rango de fechas                                          
----------------------------------------------------------------------------

-- Variables del reporte 
define _tamrep = 180
define _titrep = 'UTILIDAD REALIZADA ENTRE &3 Y EL &4'
define _codrep = 'PFFUT513'
--spool c:\porfin\pffut.inf
@@ID1PFLIBIMP.sql

--- FUNCIONES LOCALES AL INFORME

-------------------------------------------------------------------------------
create or replace function fn_contratos (neteo number
				      ,titulo number
				      ,contratos number) return number is
-------------------------------------------------------------------------------
-- Objetivo:  Devuleve los contratos del título
-------------------------------------------------------------------------------
----------------------------------------------------------------------
-- Variables locales a fn_contratos
----------------------------------------------------------------------
t_contratos	pfinversiones_v2.inv_contratos_futuro%type;
t_fraccionado	number;
t_operacion 	pfoperaciones_v2.ope_top_codigo%type;
t_compras	pfinversiones_v2.inv_contratos_futuro%type;
t_ventas 	pfinversiones_v2.inv_contratos_futuro%type;
t_tipo 		pfoperaciones_v2.ope_top_codigo%type;	
t_otroscon 	pfinversiones_v2.inv_contratos_futuro%type;
----------------------------------------------------------------------------
cursor c_contratos is
select sum(contratos) cant,operacion from
(select max(i.inv_contratos_futuro)contratos
       ,t.tit_consec titulo
       ,o.ope_top_codigo operacion
from pfneteo_v2 n
    ,pftitulos_v2 t
    ,pfinversiones_v2 i
    ,pfoperaciones_v2 o
where  net_consec = neteo
  and (n.net_titulo_compra  = t.tit_consec 
    or n.net_titulo_venta   = t.tit_consec)
  and  i.inv_consec         = t.tit_inv_consec
  and o.ope_tit_consec      = t.tit_consec
  and o.ope_top_codigo      in ('1','2')
group by t.tit_consec, o.ope_top_codigo )net
group by operacion
  ;

begin
-- Conseguimos total de compras y de ventas
for ict in c_contratos loop
	if ict.operacion = '1' then
	   t_compras := ict.cant;
	   t_operacion := '1';
	else
	   t_ventas  := ict.cant;
	   t_operacion := '2';
	end if;
end loop;
-- miramos si el título fue fraccionado
begin
select count(*) 
      ,ope_top_codigo
  into t_fraccionado
      ,t_tipo 
  from pftitulos_v2
      ,pfoperaciones_v2
 where tit_tit_consec = titulo
   and ope_tit_consec = tit_consec 
   and ope_top_codigo in ('1','2')
   group by ope_top_codigo
    ;
exception when others then
	t_fraccionado := 0;
	
	select ope_top_codigo
	  into t_tipo 
	  from pftitulos_v2
	      ,pfoperaciones_v2
	 where ope_tit_consec = tit_consec 
	   and ope_top_codigo in ('1','2')
	   and tit_consec     = titulo
	   group by ope_top_codigo
    ;
end;
-- Se busca si otro título diferente resto
begin  
select max(i.inv_contratos_futuro)contratos
  into t_otroscon
from pfneteo_v2 n
    ,pftitulos_v2 t
    ,pfinversiones_v2 i
    ,pfoperaciones_v2 o
where  net_consec = neteo
  and (n.net_titulo_compra  = t.tit_consec 
    or n.net_titulo_venta   = t.tit_consec)
  and  i.inv_consec         = t.tit_inv_consec
  and o.ope_tit_consec      = t.tit_consec
  and o.ope_top_codigo     = t_tipo
  and t.tit_consec <>titulo;
exception when others then
	t_otroscon := 0;
end;
 -- Si fue fraccionado se resta
if t_fraccionado > 0 and t_tipo = '1' then
      if t_otroscon > 0  then
	t_contratos := t_ventas - t_otroscon;
      else 
        t_contratos := t_ventas;
      end if;  
elsif t_fraccionado > 0 and t_tipo = '2' then
      if t_otroscon > 0	 then
	t_contratos := t_compras - t_otroscon ;
      else	
        t_contratos := t_compras;
      end if;	
else
-- si no es fraccionado se envia el mismo valor
	t_contratos := contratos;
end if;

return ABS(t_contratos);
end;-- De fn_contratos
----------------------------------------------------------------------------
/
show error

-- Formatos de columnas 
column consecutivo              heading 'Consecutivo Cierre'    format 99999   
column contrato                 heading 'Contrato'		format a20     	
column monto			heading 'Monto'			format 999,999,999,999.9999
column operacion		heading 'Operación'		format a10
column titulo			heading 'Título'		format 99999999
column fecha_negocio		heading 'Fecha Neg.'		format a10
column precio			heading 'Precio'		format 99,999,999.999999
column contratos_originales	heading 'Cont. Originales'	format 999,999
column contratos_cerrados	heading 'Cont. Cerrados'	format 999,999
column contratos_vigentes	heading 'Cont. Vigentes'	format 999,999
column utilidad_realizada	heading 'Utilidad Realizada'	format 999,999,999.99

-- Totalizadores
break on consecutivo skip 2 on operacion skip 1 on report

compute sum label 'Total por Contrato' of utilidad_realizada  on consecutivo
compute sum of utilidad_realizada    on consecutivo

compute sum label 'Total por Operacion' of contratos_originales  on operacion
compute sum of contratos_originales  on operacion
compute sum of contratos_cerrados    on operacion
compute sum of contratos_vigentes    on operacion

@@ID1PFLIBEXC.sql

 select net_consec                                                consecutivo  
       ,pcf_contrato                                              contrato     
       ,c.pcf_monto                                               monto
       ,decode(o.ope_top_codigo,'1','Compra','Venta')             operacion
       ,t.tit_consec                                              titulo
       ,o.ope_f_compromiso                                        fecha_negocio
       ,o.ope_precio                                              precio
       ,i.inv_contratos_futuro                                    contratos_originales
       ,fn_contratos(n.net_consec
       		    ,t.tit_consec
       		    ,i.inv_contratos_futuro)                      contratos_cerrados
       ,i.inv_contratos_futuro -
        fn_contratos(n.net_consec
       		    ,t.tit_consec
       		    ,i.inv_contratos_futuro)			  contratos_vigentes
       ,decode(o.ope_top_codigo,'2',SUM(n.net_utilidad),0)        utilidad_realizada
  from pfneteo_v2             n
      ,pftitulos_v2           t
      ,pfinversiones_v2       i
      ,pfliquidaciones_v2     l
      ,pfcontratos_futuros_v2 c
      ,pfoperaciones_v2       o
      ,pfview_portafolios_v2
 where n.net_emp_codigo       = view_emp_codigo
   and n.net_por_codigo       = view_por_codigo
   and (n.net_titulo_compra = t.tit_consec or n.net_titulo_venta   = t.tit_consec)
   and t.tit_inv_consec     = i.inv_consec
   and c.pcf_contrato       = l.liq_cuenta_futuro
   and c.pcf_tin_codigo     = l.liq_tin_codigo
   and c.pcf_cin_codigo     = l.liq_cin_codigo  
   and c.pcf_emi_codigo     = l.liq_emi_codigo
   and o.ope_tit_consec     = t.tit_consec
   and o.ope_top_codigo     in ('1','2')
   and l.liq_consec         = nvl(t.tit_liq_consec_compra,t.tit_liq_consec_venta)
   and n.net_fecha between to_date(&3,'yyyymmdd') and to_date(&4,'yyyymmdd')
   group by n.net_consec, pcf_contrato, c.pcf_monto, o.ope_top_codigo, t.tit_consec, o.ope_f_compromiso, o.ope_precio, i.inv_contratos_futuro
   order by n.net_consec,o.ope_top_codigo
;
DROP function  fn_contratos
/


--spool off
exit




