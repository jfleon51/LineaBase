-------------------------------------------------------------------------------
Prompt Compilando fbd_metodo_valoracion
-------------------------------------------------------------------------------
Create or Replace
FUNCTION "FBD_METODO_VALORACION" (PFM_TITULO NUMBER
                              ,pfm_fecha   date
                              ,pfm_cual    varchar2
                              ,pfm_muestre varchar2
                              ,pfm_mp      varchar2 default 'N'  -- Indica si se llama desde pbd_metodo_valoracion  --jga-11083102-2011/09/02
                              ) return varchar2 is
-------------------------------------------------------------------------------------
-- Objetivo: Retorna el metodo de valoracion de un titulo a una fecha dada para
--           caso de Circular 33 (Metodo QES).  Para otros metodos devuelve
--           el mismo a fecha actual del sistema
--           Dependiendo de pfm_cual retorna:
--             C= Codigo del Metodo, por ejemplo QES-TIR, QES-PL, etc.
--             N= Nombre del Metodo, por ejemplo Precio Limpio, Precio Sucio, etc.
--             V= Valor segun metodo. Por ejemplo, en QES-PL valor del precio limpio
-------------------------------------------------------------------------------------

   -------------------------------------------------------------------------
   -- Definicion de variables locales a la funcion fbd_metodo_valoracion
   -------------------------------------------------------------------------
     Cursor c_datos_titulo is
       select nvl(tit_fuente,inv_fuente)                                             fuente
             ,nvl(por_fondo,'N')                                                     fondo
             ,fbd_desc_especie_tit(tit_consec,'TIPO_PRECIO')                         tipo_precio
             ,fbd_desc_especie_tit(tit_consec,'CNV_CODIGO_PRECIO')                   cnv_codigo_precio
             ,fbd_desc_especie_tit(tit_consec,'CNV_CODIGO')                          cnv_codigo
             ,nvl(nvl(nvl(inv_f_compra,pfm_fecha),inv_f_emision),trunc(sysdate))     f_compra
             ,tit_emp_codigo                                                         empresa
             ,emp_pai_codigo                                                         pais
             ,tit_efe_dcto_vm                                                        tasa_fijo
             ,tit_precio_fijo                                                        precio_fijo
             ,tit_margen_fijo                                                        margen_fijo
             ,nvl(tit_tipo_fijo,'N')                                                 tipo_fijo
             ,tit_efe_compra_por                                                     tir
             ,tit_tirpm                                                              mercado
             ,fbd_desc_especie_tit(tit_consec,'CLASIFICACION')                       clasificacion
             ,fbd_desc_especie_tit(tit_consec,'JERARQUIA')                           jerarquia
             ,per_pai_codigo                                                         pais_emisor
             ,emp_pai_codigo                                                         pais_empresa
             ,nvl(emi_nacion,'N')                                                    emisor_nacion
             ,inv_cnv_codigo                                                         moneda_titulo
             ,emp_cnv_codigo                                                         moneda_empresa
             ,fbd_desc_especie_tit(pfm_titulo,'METODO_VALORACION')                   metodo
             ,fbd_desc_referencia ('DPT','1')                                        mexico          --jga-11090715-2011/09/07
             ,nvl(fbd_referencia('DFS','V'),'N')                                     ind_fin_semana  --jga-11090715-2011/09/07
             ,fbd_codigo_bolsa(tit_consec,'C')                                       codigo_bolsa
             ,fbd_isin(tit_consec)                                                   isin
             ,fbd_codigo_bolsa(tit_consec,'T')                                       tipo_codigo_bolsa
             ,fbd_nemotecnico(tit_consec)                                            nemotecnico
             ,inv_f_emision                                                          f_emision
             ,inv_f_vcto                                                             f_vcto
             ,fbd_car_negociacion(tit_consec,'MOD_INV')                              modalidad
             ,fbd_car_negociacion(tit_consec,'PER_INV')                              periodo
             ,decode(fbd_referencia('DPT','V')
                    ,'S',fbd_plazo(inv_f_vcto,pfm_fecha,365)
                        ,fbd_plazo(inv_f_vcto,pfm_fecha,por_anualidad)  -
                         decode(por_anualidad,'360',0
                                                   ,fbd_bisiestos(pfm_fecha,inv_f_vcto)
                               )
                    )                                                  dias_vcto
             ,decode(fbd_car_negociacion(tit_consec,'IF')
                    ,null,'FIJA'
                         ,fbd_car_negociacion(tit_consec,'IF')
                    )                                                                tasa_negociacion
             ,decode(fbd_car_negociacion(tit_consec,'IF')
                    ,null,decode(fbd_car_negociacion(tit_consec,'TN')
                                ,null,decode(fbd_car_negociacion(tit_consec,'TE')
                                            ,null,decode(fbd_modalidad(inv_consec),'D','0',to_char(inv_efe_compra_por))  --jga-11083003-2011/08/30
                                                 ,fbd_car_negociacion(tit_consec,'TE')
                                             )
                                     ,fbd_car_negociacion(tit_consec,'TN')
                                )
                         ,(decode(fbd_car_negociacion(tit_consec,'OPE')
                                 ,'*','+'
                                 ,'#','+'
                                     ,fbd_car_negociacion(tit_consec,'OPE')
                                 )||
                                 fbd_car_negociacion(tit_consec,'PTS')
                          )
                    )                                                                spread
             ,fbd_mod_descripcion(inv_consec)                                        desc_modalidad
             ,fbd_codigo_bolsa(tit_consec,'G')                                       categorizacion
             ,fbd_desc_especie_tit(tit_consec,'CNV_CODIGO')                          tasa_referencia
             ,fbd_desc_especie_tit(tit_consec,'IND_MAYOR_PESO')                      mayor_peso
             ,inv_plazo                                                              plazo
           --,decode(fbd_habil_atras(pfm_fecha,emp_pai_codigo)                                                   --jga-11101105-2011/10/11
           --       ,pfm_fecha,-1                                                                                --jga-11101105-2011/10/11
           --                 ,fbd_plazo(inv_f_vcto,fbd_habil_atras(pfm_fecha,emp_pai_codigo),por_anualidad)     --jga-11101105-2011/10/11
           --                  -                                                                                 --jga-11101105-2011/10/11
           --                  decode(por_anualidad                                                              --jga-11101105-2011/10/11
           --                        ,'360',0                                                                    --jga-11101105-2011/10/11
           --                              ,fbd_bisiestos(fbd_habil_atras(pfm_fecha,emp_pai_codigo),inv_f_vcto)  --jga-11101105-2011/10/11
           --                        )                                                                           --jga-11101105-2011/10/11
           --       )                                                                dias_vcto_ant               --jga-11101105-2011/10/11
             ,decode(fbd_referencia('DPT','V')                                                                   --jga-11101105-2011/10/11
                    ,'S',fbd_plazo(inv_f_vcto,pfm_fecha-1,365)                                                   --jga-11101105-2011/10/11
                        ,fbd_plazo(inv_f_vcto,pfm_fecha-1,por_anualidad)  -                                      --jga-11101105-2011/10/11
                         decode(por_anualidad,'360',0                                                            --jga-11101105-2011/10/11
                                                   ,fbd_bisiestos(pfm_fecha-1,inv_f_vcto)                        --jga-11101105-2011/10/11
                               )                                                                                 --jga-11101105-2011/10/11
                    )                                                                dias_vcto_ant               --jga-11101105-2011/10/11
             ,tit_margen                                                             margen_actual
             ,tit_por_codigo                                                         portafolio
             ,decode(nvl(trim(fbd_referencia('DBM','V')),'1'),1,'N',2,'S')           usar_ultimo_margen
             ,fbd_desc_especie_tit(tit_consec,'CNV_CODIGO_MARGEN')                   ind_margen  -- JCD Caso 10111828 Cir. 042
             --,null                                                ind_margen  -- JCD Caso 10111828 Cir. 042
             ,tit_cin_codigo                                                         cin  --jga-11061516-2011/08/03
             ,tit_tin_codigo                                                         tin  --jga-11061516-2011/08/03
             ,tit_emi_codigo                                                         emi  --jga-11061516-2011/08/03
             ,inv_ems_codigo                                                         ems  --jga-11061516-2011/08/03
             ,inv_ser_codigo                                                         ser  --jga-11061516-2011/08/03
             ,nvl(tit_valora_mp,'N')                                                 valora_mp      -- Indica que valoró por precio                  --jga-11083102-2011/09/01
             ,nvl(tit_valora_mp_ant,'N')                                             valora_mp_ant  -- Indica que valoró por precio el día anterior  --jga-11083102-2011/09/01
             ,nvl(tit_metodo_valoracion_ant,tit_metodo_valoracion)                 metodo_ant  -- ORC 20111021
         from pfempresas_v2
             ,pfpersonas_v2
             ,pfemisores_v2
             ,pfportafolios_v2
             ,pfinversiones_v2
             ,pftitulos_v2
        where emp_codigo     = por_emp_codigo
          and per_ident      = emi_per_ident
          and emi_codigo     = inv_emi_codigo
          and por_emp_codigo = inv_emp_codigo
          and por_codigo     = inv_por_codigo
          and inv_consec     = tit_inv_consec
          and tit_consec     = pfm_titulo
       ;

   -------------------------------------------------------------------------
   -- Definicion de variables locales a la funcion fbd_metodo_valoracion
   -------------------------------------------------------------------------
     t_nombre_metodo         pfreferencias_v2.ref_descripcion%type;
     t_nombre_fuente         pfreferencias_v2.ref_descripcion%type;
     t_nombre_clasificacion  pfreferencias_v2.ref_descripcion%type;
     t_valor                 pfreferencias_v2.ref_descripcion%type;
     t_valora_mp             pftitulos_v2.tit_valora_mp%type;  --jga-11083102-2011/09/02
----------------------------------------------------------------------------

begin -- De fbd_metodo_valoracion
  for icd in c_datos_titulo loop

    begin
      select fte.ref_descripcion  ,cla.ref_descripcion     ,met.ref_descripcion
        into t_nombre_fuente      ,t_nombre_clasificacion  ,t_nombre_metodo
        from pfreferencias_v2    fte
            ,pfreferencias_v2    cla
            ,pfreferencias_v2    met
       where fte.ref_codigo(+) = icd.fuente
         and cla.ref_codigo(+) = icd.clasificacion
         and met.ref_codigo(+) = icd.metodo
      ;
      exception when others then t_nombre_fuente := null; t_nombre_clasificacion := null;  t_nombre_metodo := null;
    end;

    if pfm_mp = 'S' then  --jga-11083102-2011/09/02
      t_valora_mp := icd.valora_mp;
    else
      t_valora_mp := icd.valora_mp_ant;
    end if;
    t_valor  := fbd_metodo_valoracion_papel (pfm_fecha
                                            ,pfm_cual
                                            ,pfm_muestre
                                            ,icd.fuente
                                            ,icd.fondo
                                            ,icd.tipo_precio
                                            ,icd.cnv_codigo_precio
                                            ,icd.cnv_codigo
                                            ,icd.f_compra
                                            ,icd.empresa
                                            ,icd.pais
                                            ,icd.tasa_fijo
                                            ,icd.precio_fijo
                                            ,icd.margen_fijo
                                            ,icd.tipo_fijo
                                            ,icd.tir
                                            ,icd.mercado
                                            ,icd.clasificacion
                                            ,icd.jerarquia
                                            ,icd.pais_emisor
                                            ,icd.pais_empresa
                                            ,icd.emisor_nacion
                                            ,icd.moneda_titulo
                                            ,icd.moneda_empresa
                                            ,t_nombre_fuente
                                            ,t_nombre_clasificacion
                                            ,icd.metodo
                                            ,t_nombre_metodo
                                            ,icd.mexico
                                            ,icd.ind_fin_semana
                                            ,icd.codigo_bolsa
                                            ,icd.tipo_codigo_bolsa
                                            ,icd.nemotecnico
                                            ,icd.isin
                                            ,icd.f_emision
                                            ,icd.f_vcto
                                            ,icd.modalidad
                                            ,icd.desc_modalidad
                                            ,icd.periodo
                                            ,icd.dias_vcto
                                            ,icd.tasa_negociacion
                                            ,icd.spread
                                            ,'S' -- Buscar margen Propio
                                            ,icd.usar_ultimo_margen
                                            ,icd.categorizacion
                                            ,icd.plazo
                                            ,icd.tasa_referencia
                                            ,icd.mayor_peso
                                            ,icd.dias_vcto_ant
                                            ,icd.margen_actual
                                            ,icd.portafolio
                                            ,pfm_titulo
                                            ,pfm_titulo
                                            ,icd.ind_margen  -- JCD Caso 10111828 Cir. 042
                                            ,icd.cin  --jga-11061516-2011/08/03
                                            ,icd.tin  --jga-11061516-2011/08/03
                                            ,icd.emi  --jga-11061516-2011/08/03
                                            ,icd.ems  --jga-11061516-2011/08/03
                                            ,icd.ser  --jga-11061516-2011/08/03
                                            ,t_valora_mp  -- Indica que valoró por precio el día anterior  --jga-11083102-2011/09/01
                                            ,icd.metodo_ant
                                            );
  end loop;
  return(t_valor);

end; -- de fbd_metodo_valoracion
----------------------------------------------------------------------------







/
-------------------------------------------------------------------------------
Show err
