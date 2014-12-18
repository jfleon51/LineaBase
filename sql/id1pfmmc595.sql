----------------------------------------------------------------------------
-- Nombre    : PFMMC595 (Dito Contable de un titulo) 
-- Parametros: Empresa, Portafolio, Titulo, Rango de Fechas
----------------------------------------------------------------------------

-- Variables del reporte 
define _tamrep = 180
define _titrep = 'DITO DEL TITULO &3 ENTRE EL &4 Y EL &5'
define _codrep = 'PFMMC595'

@@id1pflibimp.sql

-- Formatos de columnas 
column tipo        heading 'Tipo Valor'    format a12
column operacion   heading ' '             format a1 noprint
column cuenta      heading 'Cuenta'        format a32
column transac     heading 'Transaccion'   format a25
column fecha       heading 'Fecha'         format a10
column debitos     heading 'Debitos'       format 99,999,999,999,999.99
column creditos    heading 'Creditos'      format 99,999,999,999,999.99
column descripcion heading 'Descripcion'   format a70
-- Definicion de variables propias al informe
var t_consec number;
execute :t_consec := to_number(ltrim(rtrim('&3'))); 
-- Rompimientos
break on tipo skip 2 on transac skip 1 on fecha skip 1 on report

compute sum of debitos  on tipo
compute sum of creditos on tipo
compute sum of debitos  on report
compute sum of creditos on report
compute sum of debitos  on transac
compute sum of creditos on transac
compute sum of debitos  on fecha
compute sum of creditos on fecha
@@id1pflibexc.sql

  select decode(mco_tipo_valor
               ,'$','Pesos'
               ,'U','Moneda'
               ,'N','Vr.PN'
               ,'C','Vr.TRM'
               ,'D','Vr.PN-Vr.TRM'
               ,'T','PN'
               ,'F','TRM'
               ,'R','PN-TRM'
               )                                         Tipo
        ,decode(por_activo_pasivo,'E',top_desc_pasivo
                                     ,top_descripcion)   transac
        ,to_char(mco_ope_fecha,'yyyy/mm/dd')             fecha
        ,mco_cuenta||mco_auxiliar1                       cuenta
        ,decode(mco_naturaleza,'D',mco_valor,null)       debitos
        ,decode(mco_naturaleza,'C',mco_valor,null)       creditos
        ,decode(mco_concepto
               ,null,mco_descripcion
                    ,ltrim(to_char(mco_porcentaje)) ||
                     '% de '||'('||mco_tipo_concepto||
                     mco_signo||decode(length(mco_concepto),2,mco_concepto,'('||mco_concepto||')')||') '        ||
                     decode(mco_tipo_concepto
                           ,'F',cco_nombre||decode(length(mco_concepto),2,null,'...')
                           ,'T',ctb_nombre||decode(length(mco_concepto),2,null,'...')
                           )
               )                                         descripcion
    from pftipo_operaciones_v2
        ,pfconceptos_tributarios_v2
        ,pfconceptos_contables_v2
        ,pfportafolios_v2
        ,pfmovtos_contables_v2
   where top_codigo        = mco_top_codigo
     and ctb_codigo    (+) = mco_concepto
     and cco_tipo_movto(+) = mco_clase
     and cco_contexto  (+) = mco_tipo_concepto
     and cco_codigo    (+) = substr(mco_concepto,1,2)
     and mco_ope_fecha between to_date('&4','YYYYMMDD') and to_date('&5','YYYYMMDD')
     and mco_tit_consec    = :t_consec
     and por_emp_codigo = mco_emp_codigo
     and por_codigo     = mco_por_codigo
order by 1,2,3,mco_consec
/

-- Resumir el movimiento por cuenta

-- Formatos de columnas 
column cuenta     heading 'Cuenta'      format a16
column debitos    heading 'Debitos'     format 99,999,999,999,999.99
column m_debitos  heading 'M.Debitos'   format 99,999,999,999,999.99
column creditos   heading 'Creditos'    format 99,999,999,999,999.99
column m_creditos heading 'M.Creditos'  format 99,999,999,999,999.99

break on report
compute sum of debitos    on report
compute sum of m_debitos  on report
compute sum of m_creditos on report
compute sum of creditos   on report

 select  mco_cuenta||mco_auxiliar1                   cuenta
        ,sum(decode(mco_naturaleza,'D',mco_valor,0)) m_debitos
        ,sum(decode(mco_naturaleza,'C',mco_valor,0)) m_creditos
        ,decode(sign(sum(decode(mco_naturaleza
                                       ,'D',mco_valor
                                       ,'C',-mco_valor
                                       )
                                )
                            )
                       ,1,sum(decode(mco_naturaleza
                                     ,'D',mco_valor
                                    ,'C',-mco_valor
                                    )
                             )
                          ,null
                      )                             debitos
         ,decode(sign(sum(decode(mco_naturaleza
                                       ,'D',mco_valor
                                       ,'C',-mco_valor
                                       )
                                )
                             )
                       ,-1,-sum(decode(mco_naturaleza
                                      ,'D',mco_valor
                                      ,'C',-mco_valor
                                      )
                               )
                          ,null)                     creditos
    from pfmovtos_contables_v2 
   where mco_ope_fecha between to_date('&4','YYYYMMDD') and to_date('&5','YYYYMMDD')
     and mco_tit_consec = :t_consec
group by mco_cuenta||mco_auxiliar1 
/


exit

