-------------------------------------------------------------------------------
Prompt Compilando fbd_margen_propio
-------------------------------------------------------------------------------
Create or Replace
FUNCTION            "FBD_MARGEN_PROPIO" (PFM_TITULO  NUMBER    -- CONSEC TITULO
                          ,pfm_fecha   date      -- Fecha de evaluacion
                          ,pfm_tipcal  varchar2  -- Tipo Calculo (C)ompra o (V)enta o (M)Valoración Mercado  --jga-11061516-2011/07/27
                          )  return number is
----------------------------------------------------------------------------
-- Calcula el margen del titulo recibido como parametro con base en la
-- circular 003 de la Superbancaria sea para compra o para venta
----------------------------------------------------------------------------
   type vec_plazo    is table of pfinversiones_v2.inv_plazo%type
                                 index by binary_integer;
   type vec_tasa     is table of pfinversiones_v2.inv_efe_compra%type
                                 index by binary_integer;
   type vec_nominal is table of pfinversiones_v2.inv_vr_nominal%type
                                 index by binary_integer;

   type vec_ope     is table of pfoperaciones_v2.ope_consec%type
                                index by binary_integer;
   type vec_val     is table of pfoperaciones_v2.ope_valor%type
                                index by binary_integer;
   type vec_fec     is table of pfoperaciones_v2.ope_fecha%type
                                index by binary_integer;


   -------------------------------------------------------------------------
   -- Vectores para guardar los valores de la operaciones
   -------------------------------------------------------------------------
   va_vr_teo     vec_nominal        ; -- Valor de la operacion de mercado
   va_ta_teo     vec_tasa           ; -- Tasa de mercado del titulo
   va_dias       vec_plazo          ; -- Numero de dias de plazo
   va_idx        vec_plazo          ; -- Numero de flujo en operaciones
   va_vr_mer     vec_nominal        ; -- Valor de la operacion de mercado
   va_ta_mer     vec_tasa           ; -- Tasa de mercado del titulo
   max_va        binary_integer := 0; -- Dimensionamiento de va
   ix_plazo      vec_plazo          ; -- Plazo del tramo de la implicita
   ix_tasa       vec_tasa           ; -- Tasa implicita del tramo
   max_ix        binary_integer := 0; -- Control del numero maximo de flujos
   max_vao       binary_integer := 0; -- Dimensionamiento de va
   va_ope        vec_ope            ; -- Consecutivos de operacion
   va_fec        vec_fec            ; -- Fechas de operacion
   va_val        vec_val            ; -- Valores de operacion

   -------------------------------------------------------------------------
   glo_vcto          pfinversiones_v2.inv_f_compra%type   ; -- Fecha vcto titulo
   glo_tasa          pfinversiones_v2.inv_efe_compra%type ; -- Tasa de compra
   glo_estado        pfinversiones_v2.inv_estado%type     ; -- Estado del titulo
   glo_anualidad     pfinversiones_v2.inv_anualidad%type  ; -- Anualidad del papel
   glo_inversion     pfinversiones_v2.inv_consec%type     ; -- Inversion
   glo_vr_compra     pfinversiones_v2.inv_vr_compra%type  ; -- Vr de compra
   glo_vr_venta      pfinversiones_v2.inv_vr_compra%type  ; -- Vr de compra
   glo_vr_mercado    pftitulos_v2.tit_vppm%type           ; -- Vr mercado anterior  --jga-11061516-2011/07/27
   t_tir             pfinversiones_v2.inv_efe_compra%type:=0  ; --Tasa prueba
   t_tire            pfinversiones_v2.inv_efe_compra%type:=0  ; --Tasa efectiva
   t_plazo           pfinversiones_v2.inv_plazo%type      ; -- Plazo al vcto papel
   glo_metodo        pfreferencias_v2.ref_codigo%type     ; -- Metodo de valoracion
   t_pos             binary_integer                   := 0; -- Para vectores
   glo_bisiesto      pfespecies_v2.esp_bisiesto%type;
   glo_margen        pfinversiones_v2.inv_efe_compra%type:=0  ; --Tasa prueba
   t_anualidad       pfinversiones_v2.inv_anualidad%type:=null; -- Anualidad
   glo_muestre       varchar2(1) := 'S';
   t_inversion       pfinversiones_v2.inv_consec%type;
   t_f_compra        date;
   t_estado          pfinversiones_v2.inv_estado%type;

   -------------------------------------------------------------------------

    cursor c_operaciones_todas is
      select ope_consec
            ,ope_valor
            ,ope_f_recalculo
        from pfoperaciones_v2
       where ope_tit_consec     = pfm_titulo
    ;

  --------------------------------------------------------------------------------------------


  FUNCTION fn_tasa_referencia(pft_inversion   pfinversiones_v2.inv_consec%type
                             ,pft_titulo      pftitulos_v2.tit_consec%type
                             ,pft_exv_consec  pfoperaciones_v2.ope_exv_consec%type
                             ,pft_ind_valora  pfespecies_v2.esp_cnv_codigo%type
                             ,pft_fecha       date
                             ,pft_moneda_emp  pfempresas_v2.emp_cnv_codigo%type
                             ,pft_dias_vcto   pfinversiones_v2.inv_plazo%type
                             ,pft_dias_flu    pfinversiones_v2.inv_plazo%type
                             ,pft_fondo       pfportafolios_v2.por_fondo%type
                             ,pft_pais        pfempresas_v2.emp_pai_codigo%type
                             ,pft_f_compro    pfinversiones_v2.inv_f_compromiso%type
                             ,pft_f_compra    pfinversiones_v2.inv_f_compra%type
                             ,pft_inv_anual   pfinversiones_v2.inv_anualidad%type
                             ,pft_tipo_fijo   pftitulos_v2.tit_tipo_fijo%type
                             ,pft_tasa_fija   pftitulos_v2.tit_efe_dcto_vm%type
                             ) return number is
  ----------------------------------------------------------------------------------------------
  -- Determina el valor del indicador para una inversión en una fecha dada.
  -------------------------------------------------------------------------
  -- Variables locales a la funcion
  -------------------------------------------------------------------------
     t_ind_genera          pfconversiones_v2.cnv_codigo%type        ; -- Indicador a recibir de negociacion
     t_ind_desta           pfconversiones_v2.cnv_codigo%type        ; -- Indicador a recibir de descuento
     t_operador            pfvar_inversion_v2.var_operador_desv%type; -- Op. ind. fin.
     t_desviacion          pfvar_inversion_v2.var_desviacion%type   ; -- Desviacion
     t_modalidad           pfmodalidades_v2.mod_modalidad%type      ; -- Modalidad
     t_periodo             pfinversiones_v2.inv_plazo%type          ; -- Periodicidad
     t_ind_curva_tir       pfconversiones_v2.cnv_codigo%type        ;
     t_dias_curva_tir      pfconversiones_v2.cnv_dias_reporte%type  ;
     t_refe                number                                   ;
     va_tasa               pfinversiones_v2.inv_efe_compra%type     ;
     glo_indice            varchar2(1)                              ;

  -------------------------------------------------------------------------
  -- Cursor con la modalidad del indicador
  -------------------------------------------------------------------------
     cursor c_mod_referencia(p_indicador varchar2) is
        select ref_valor         periodicidad
              ,cnv_mod_modalidad modalidad
              ,cnv_metodo        metodo_indicador
              ,cnv_base_efectivizar base_indicador             -- 12051110 OVG 20120512
          from pfreferencias_v2
              ,pfconversiones_v2
         where ref_codigo = cnv_mod_periodo
           and cnv_codigo = p_indicador
     ;
  -------------------------------------------------------------------------
  Begin
   --

   --pbd_mensaje('TR('||t_ind_desta||') = '||to_char(va_tasa,'999.999')||' Ef.',pft_titulo);
   --pbd_mensaje('pft_titulo>'||pft_titulo,pft_titulo);
   --pbd_mensaje('pft_inv_anual>'||pft_inv_anual,pft_titulo);


     pbd_indicador(pft_inversion    -- Consec Inversion
                  ,pft_exv_consec   -- Consec ofl
                  ,t_ind_genera     -- Indicador a recibir de negociacion
                  ,t_ind_desta      -- Indicador a recibir de descuento
                  ,t_operador       -- Oper Desv a recibir
                  ,t_desviacion     -- Desviacion a recibir
                  ,t_modalidad      -- Modalidad de la inversion a recibir
                  ,t_periodo        -- Period de la inversion a recibir
                  );
     t_ind_genera := nvl(t_ind_genera,t_ind_desta);

   --pbd_mensaje('t_ind_desta>'||t_ind_desta,pft_titulo);

     if t_ind_desta is not null then
       begin
         select 'S'
           into glo_indice
           from pfconversiones_v2
          where cnv_codigo = t_ind_desta
            and cnv_tipo   = 'WIV'
         ;
         exception when others then glo_indice := 'N';
       end;
     end if;
     if glo_indice = 'S' then
       va_tasa := fbd_unidad_en_pesos(pft_ind_valora,pft_fecha,pft_moneda_emp);
       if glo_muestre = 'S' then
          pbd_mensaje('TR('||t_ind_desta||') = '||to_char(va_tasa,'999.999')||' Ef.',pft_titulo);
       end if;
     else
       for icm in c_mod_referencia(t_ind_desta) loop
          if icm.metodo_indicador = 'C' then
             -- Determinar si existe parametrizada una curva TIR
             t_ind_curva_tir  := fbd_desc_especie_tit(pft_titulo,'CNV_FUTUROS');
             -- Determina el numero de dias parametrizado para la curva tir
             if t_ind_curva_tir  is not null then
                t_dias_curva_tir := to_number(fbd_desc_conversion(t_ind_curva_tir,'DIAS_TIR'));
             end if;
             -- Si dias al vcto < dias curva tir y se tiene curva tir, se asigna el valor de la curva para dias al vcto
             if pft_dias_vcto <= t_dias_curva_tir and t_ind_curva_tir is not null then
                if pft_fondo = 'S' then
                   if glo_muestre = 'S' then
                      pbd_mensaje('Tasa Curva TIR, obtención a través de Betas con '||pft_dias_vcto||' días.',pft_titulo);
                   end if;
                   va_tasa := fbd_valor_cero_cupon(t_ind_curva_tir,pft_dias_vcto,greatest(nvl(pft_f_compro,pft_f_compra),
                   fbd_habil_atras(pft_fecha-1,pft_pais)),'CT');
                   if glo_muestre = 'S' then
                      pbd_mensaje('Fecha: '||to_char(pft_fecha-1,'yyyy/mm/dd')||' TR('||t_ind_curva_tir||') = '||to_char(va_tasa,'999.999')||' Ef.',pft_titulo);
                   end if;
                else
                   if glo_muestre = 'S' then
                      pbd_mensaje('Tasa Curva TIR, obtención a través de Betas con '||pft_dias_vcto||' días.',pft_titulo);
                   end if;
                   va_tasa := fbd_valor_cero_cupon(t_ind_curva_tir,pft_dias_vcto,fbd_habil_atras(pft_fecha,pft_pais),'CT');
                   if glo_muestre = 'S' then
                      pbd_mensaje('Fecha: '||to_char(pft_fecha-1,'yyyy/mm/dd')||' TR('||t_ind_curva_tir||') = '||to_char(va_tasa,'999.999')||' Ef.',pft_titulo);
                   end if;
                end if;
             else    -- de lo contrario se mantiene con la curva cero cupon.
                if pft_fondo = 'S' then
                   if glo_muestre = 'S' then
                      pbd_mensaje('Tasa Cero Cupón, obtención a través de Betas con '||pft_dias_flu||' días.',pft_titulo);
                   end if;
                   va_tasa := fbd_valor_cero_cupon(t_ind_desta,pft_dias_flu,greatest(nvl(pft_f_compro,pft_f_compra),
                   fbd_habil_atras(pft_fecha-1,pft_pais)),'CC');
                else
                   if glo_muestre = 'S' then
                      pbd_mensaje('Tasa Cero Cupón, obtención a través de Betas con '||pft_dias_flu||' días.',pft_titulo);
                   end if;
                   va_tasa := fbd_valor_cero_cupon(t_ind_desta,pft_dias_flu,fbd_habil_atras(pft_fecha,pft_pais),'CC');
                end if;
                if glo_muestre = 'S' then
                   pbd_mensaje('TR('||t_ind_desta||') = '||to_char(va_tasa,'999.999')||' Ef. ',pft_titulo);
                end if;
             end if;
          else
            t_refe := fbd_unidad_en_pesos(t_ind_desta,pft_fecha,pft_moneda_emp);

            --pbd_mensaje('t_refe>'||t_refe,pft_titulo);

            /*va_tasa := fbd_nom_efe(t_refe
                                  ,icm.modalidad
                                  ,icm.periodicidad
                                  ,pft_inv_anual
                                          );
                                          */
            pbd_mensaje('va_tasa>'||va_tasa,pft_titulo);
            pbd_mensaje('pft_inv_anual>'||pft_inv_anual,pft_titulo);
            pbd_mensaje('icm.base_indicador>'||icm.base_indicador,pft_titulo);

            -- 12051110 OVG 20120512
            if fbd_cliente in ('o29') then -- Suramericana
              /*if (icm.base_indicador is not null and pft_inv_anual <> icm.base_indicador) then
                 va_tasa := fbd_nom_efe_ind(t_refe
                                        ,icm.modalidad
                                        ,icm.periodicidad
                                        ,pft_inv_anual
                                        ,icm.base_indicador
                                                );
              pbd_mensaje('va_tasa en 1>'||va_tasa,pft_titulo);

              else*/
                 va_tasa := fbd_nom_efe(t_refe
                                        ,icm.modalidad
                                        ,icm.periodicidad
                                        ,pft_inv_anual
                                                );
              --end if;
            else

                  va_tasa := fbd_nom_efe(t_refe
                                        ,icm.modalidad
                                        ,icm.periodicidad
                                        ,pft_inv_anual
                                                );
            end if;

            pbd_mensaje('va_tasa>'||va_tasa,pft_titulo);

            if glo_muestre = 'S' then
               pbd_mensaje('TR('||t_ind_desta||') = '||to_char(t_refe,'999.999')||' '||icm.modalidad||icm.periodicidad||'-'||pft_inv_anual||' => '||to_char(va_tasa,'999.999')||' Ef.',pft_titulo);
            end if;
            if    pft_tipo_fijo = 'T' then
              va_tasa := nvl(pft_tasa_fija,0);
              if glo_muestre = 'S' then
                 pbd_mensaje('Tasa de descuento fija para el título: '||to_char(va_tasa,'999.999')||' Ef., ',pft_titulo);
              end if;
            elsif pft_tipo_fijo = 'A' then
              va_tasa := nvl(pft_tasa_fija,0);
              if glo_muestre = 'S' then
                 pbd_mensaje('Tasa de descuento fija para el título  : '||to_char(va_tasa,'999.999')||' Ef., ',pft_titulo);
              end if;
            end if;
          end if;
       end loop;
     end if;
     --
     return(va_tasa);
   --
  end;

   --------------------------------------------------------------------------------
   PROCEDURE pr_cargue_operaciones_compra is
   --------------------------------------------------------------------------------
   -- Carga las operaciones asociadas al titulo recibido como parametro
   --------------------------------------------------------------------------------
     ------------------------------------------------------------------------------
     -- Es importante destacar que independiente del metodo que se hubiera
     -- tomado para calculo (Futuras Implicias o Proyeccion Simple), los
     -- valores del flujo ya fueron reevaluarlos antes del llamado a esta funcion.
     ------------------------------------------------------------------------------
     cursor c_tasas is
       select inv_consec                      inversion
             ,tit_vr_compra                   vr_compra
             ,fbd_desc_especie_inv(inv_consec,'CNV_CODIGO') ind_val
             ,emp_vigila                      vigila
             ,emp_codigo                      empresa
             ,emp_cnv_codigo                  moneda_empresa
             ,emp_pai_codigo                  pais_empresa
             ,por_fondo                       fondo
             ,inv_cin_codigo                  clase
             ,inv_tin_codigo                  tipo
             ,inv_emi_codigo                  emisor
             --,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad) dias_vcto
             ,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad)-
              fbd_incluye_bisiestos(pfm_fecha,nvl(tit_f_venta,inv_f_vcto),tit_consec) dias_vcto
             ,inv_f_compromiso                f_compromiso
             ,inv_f_compra                    f_compra
             ,tit_tipo_fijo                   tipo_fijo
             ,tit_efe_dcto_vm                 tasa_fija
             ,inv_f_vigencia                   vencimiento
         from pfempresas_v2
             ,pfportafolios_v2
             ,pfespecies_v2
             ,pfinversiones_v2
             ,pftitulos_v2
        where emp_codigo     = por_emp_codigo
          and por_emp_codigo = inv_emp_codigo
          and por_codigo     = inv_por_codigo
          and esp_cin_codigo = inv_cin_codigo
          and esp_tin_codigo = inv_tin_codigo
          and esp_emi_codigo = inv_emi_codigo
          and inv_consec     = tit_inv_consec
          and tit_consec     = pfm_titulo
      ;
     ----------------------------------------------------------------------
     -- Operaciones asociadas al papel recibido como parametro
     ----------------------------------------------------------------------
      cursor c_operaciones is
        select ope_fecha                   fecha
              ,ope_top_codigo              operacion
              ,decode(nvl(top_signo,'+')
                     ,'+',ope_valor
                         ,-ope_valor
                     ) *
               decode(ope_flujo_teorico
                     ,'S',1
                         ,0
                     )                     vr_flujo
              ,ope_exv_consec              flujo
          from pftipo_operaciones_v2
              ,pfoperaciones_v2
         where top_codigo         = ope_top_codigo
           and ope_flujo_teorico  = 'S'
           and ope_tit_consec     = pfm_titulo
           and ope_fecha         >  pfm_fecha
           and top_aplica_valoracion = 'S'
      ;
     ----------------------------------------------------------------------
     -- Variables locales al procedimiento
     ----------------------------------------------------------------------
     t_ind_genera  pfconversiones_v2.cnv_codigo%type        ;
     t_ind_desta   pfconversiones_v2.cnv_codigo%type        ;
     t_operador    pfvar_inversion_v2.var_operador_desv%type; -- Op. ind. fin.
     t_desviacion  pfvar_inversion_v2.var_desviacion%type   ; -- Desviacion
     t_modalidad   pfmodalidades_v2.mod_modalidad%type      ; -- Modalidad
     t_periodo     pfinversiones_v2.inv_plazo%type          ; -- Periodicidad
     t_incluir     pfoperaciones_v2.ope_incluir_real%type   ;
     t_descuento   pfoperaciones_v2.ope_valor%type          ;
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_operaciones_compra
     max_va      :=  0;
     t_incluir   := 'N';
     t_descuento :=  0;
     for ct in c_tasas loop -- Para dejar en contexto informacion titulo
       -- Si en la operacion de compra se acordo que los intereses son para el comprador
       -- entonces estos deben ser descontados del valor de compra para el calculo del margen
       begin
          -- determina si la compra incluyo o no los intereses
          select nvl(ope_incluir_real,'N')
            into t_incluir
            from pfoperaciones_v2
           where ope_tit_consec     = pfm_titulo
             and ope_top_codigo||'' = '1';
          exception when no_data_found then t_incluir := 'N';
       end;
       if t_incluir = 'S' then
          -- si la compra incluyo los intereses los descuenta del valor de compra
          -- para que el calculo del margen se realice con los flujos futuros a la operacion
          begin
             select nvl(sum(ope_valor),0)
               into t_descuento
               from pfportafolios_v2
                   ,pfoperaciones_v2
                   ,pfinversiones_v2
                   ,pftitulos_v2
              where por_emp_codigo  = tit_emp_codigo
                and por_codigo      = tit_por_codigo
                and inv_consec      = tit_inv_consec
                and tit_consec      = ope_tit_consec
                and ope_top_codigo in ( '4','5')
                and ope_fecha       = pfm_fecha
                and ope_tit_consec  = pfm_titulo;
             exception when no_data_found then t_descuento := 0;
          end;
       end if;

       glo_vr_compra := ct.vr_compra - t_descuento;  --- fbd_rendimiento_dia(pfm_fecha,pfm_titulo);
       if glo_muestre = 'S' then
          pbd_mensaje('Valor de Compra titulo :    '||to_char(ct.vr_compra,'999,999,999,999.99'),pfm_titulo);
          pbd_mensaje('Valor intereses del dia :   '||to_char(t_descuento,'999,999,999,999.99'),pfm_titulo);
          pbd_mensaje('Valor base calculo margen : '||to_char(glo_vr_compra,'999,999,999,999.99'),pfm_titulo);
       end if;

       for co in c_operaciones loop
         max_va            := max_va+1;
         va_dias(max_va)   := fbd_plazo(co.fecha,pfm_fecha,t_anualidad);
         -- Descontar bisiestos que equivale a incluir bisiestos en el cálculo del plazo  --jga-11061516-2011/07/27
         if t_anualidad = '365' then  --jga-11061516-2011/07/27
           --va_dias(max_va) := va_dias(max_va) - nvl(fbd_bisiestos(pfm_fecha,co.fecha),0);
           va_dias(max_va) := va_dias(max_va) - nvl(fbd_incluye_bisiestos(pfm_fecha,co.fecha,pfm_titulo),0);  -- 17-02-2012
         end if;
         va_vr_mer(max_va) := co.vr_flujo;
         va_ta_mer(max_va) := fn_tasa_referencia(ct.inversion
                                                ,pfm_titulo
                                                ,co.flujo
                                                ,ct.ind_val
                                                ,pfm_fecha
                                                ,ct.moneda_empresa
                                                ,ct.dias_vcto
                                              --,fbd_plazo(co.fecha,pfm_fecha,t_anualidad)  --jga-11061516-2011/07/27
                                                ,va_dias(max_va)                            --jga-11061516-2011/07/27
                                                ,ct.fondo
                                                ,ct.pais_empresa
                                                ,ct.f_compromiso
                                                ,ct.f_compra
                                                ,t_anualidad     -- anualidad portafolio
                                                ,ct.tipo_fijo
                                                ,ct.tasa_fija
                                                );
         if glo_muestre = 'S' then
            pbd_mensaje('Dias: '||to_char(va_dias(max_va),'9999')||' Flujo: '||to_char(va_vr_mer(max_va),'999,999,999,999.99')||' Tasa: '||to_char(va_ta_mer(max_va),'99.9999'),pfm_titulo);
         end if;
       end loop;
     end loop;
   End; -- De pr_cargue_operaciones_compra

   -------------------------------------------------------------------------
   PROCEDURE pr_cargue_operaciones_venta is
   -------------------------------------------------------------------------
   -- Carga las operaciones asociadas al titulo recibido como parametro
   -------------------------------------------------------------------------
     ----------------------------------------------------------------------
     -- Tasas asociadas al titulo recibido como parametro
     ----------------------------------------------------------------------
     cursor c_tasas is
       select inv_consec                      inversion
             ,tit_vr_venta                    vr_venta
             ,fbd_desc_especie_inv(inv_consec,'CNV_CODIGO') ind_val
             ,emp_vigila                      vigila
             ,emp_codigo                      empresa
             ,emp_cnv_codigo                  moneda_empresa
             ,emp_pai_codigo                  pais_empresa
             ,por_fondo                       fondo
             ,inv_cin_codigo                  clase
             ,inv_tin_codigo                  tipo
             ,inv_emi_codigo                  emisor
             --,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad) dias_vcto
             ,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad)-
              fbd_incluye_bisiestos(pfm_fecha,nvl(tit_f_venta,inv_f_vcto),tit_consec) dias_vcto
             ,inv_f_compromiso                f_compromiso
             ,inv_f_compra                    f_compra
             ,tit_tipo_fijo                   tipo_fijo
             ,tit_efe_dcto_vm                 tasa_fija
             ,inv_f_vigencia                   vencimiento
         from pfempresas_v2
             ,pfportafolios_v2
             ,pfespecies_v2
             ,pfinversiones_v2
             ,pftitulos_v2
        where emp_codigo     = por_emp_codigo
          and por_emp_codigo = inv_emp_codigo
          and por_codigo     = inv_por_codigo
          and esp_cin_codigo = inv_cin_codigo
          and esp_tin_codigo = inv_tin_codigo
          and esp_emi_codigo = inv_emi_codigo
          and inv_consec     = tit_inv_consec
          and tit_consec     = pfm_titulo
      ;
     ----------------------------------------------------------------------
     -- Operaciones asociadas al papel recibido como parametro
     ----------------------------------------------------------------------
      cursor c_operaciones is
        select ope_fecha                   fecha
              ,ope_top_codigo              operacion
              ,decode(nvl(top_signo,'+')
                     ,'+',ope_valor
                         ,-ope_valor
                     ) *
               decode(ope_flujo_teorico
                     ,'S',1
                         ,0
                     )                     vr_teo
              ,ope_exv_consec              flujo
          from pftipo_operaciones_v2
              ,pfoperaciones_v2
         where top_codigo         = ope_top_codigo
           and ope_flujo_teorico  = 'S'
           and ope_tit_consec     = pfm_titulo
           and ope_fecha         >  pfm_fecha
           and top_aplica_valoracion = 'S'
      ;
     ----------------------------------------------------------------------
     -- Variables locales al procedimiento
     ----------------------------------------------------------------------
     t_modalidad  varchar2(1) ; -- Modalidad de la inversion
     t_periodo    number(4)   ; -- Periodicidad de la inversion
     t_incluir    varchar2(1) ; -- Indicador de si incluye los inteses del dia de la venta
     t_descuento  number(16,4); -- Valor intereses en el dia de la venta para ser incluidos
     ----------------------------------------------------------------------
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_operaciones_venta
     max_va       := 0;
     t_incluir    := 'N';
     t_descuento  := 0;
     for ct in c_tasas loop -- Para dejar en contexto informacion titulo
       -- Si en la operacion de venta se acordo que los intereses son para el vendedor
       -- entonces estos deben ser descontados del valor de venta para el calculo del margen
       begin
          -- determina si la venta incluyo o no los intereses
          select nvl(ope_incluir_real,'N')
            into t_incluir
            from pfoperaciones_v2
           where ope_tit_consec     = pfm_titulo
             and ope_top_codigo||'' = '2';
          exception when no_data_found then t_incluir := 'N';
       end;
       if t_incluir = 'S' then
          -- si la venta incluyo los intereses los descuenta del valor de venta
          -- para que el calculo del margen se realice con los flujos futuros a la operacion
          begin
             select nvl(sum(ope_valor),0)
               into t_descuento
               from pfportafolios_v2
                   ,pfoperaciones_v2
                   ,pfinversiones_v2
                   ,pftitulos_v2
              where por_emp_codigo  = tit_emp_codigo
                and por_codigo      = tit_por_codigo
                and inv_consec      = tit_inv_consec
                and tit_consec      = ope_tit_consec
                and ope_top_codigo in ( '4','5')
                and ope_fecha       = pfm_fecha
                and ope_tit_consec  = pfm_titulo;
             exception when no_data_found then t_descuento := 0;
          end;
       end if;
       glo_vr_venta := ct.vr_venta - t_descuento; -- Valor de venta titulo
       if glo_muestre = 'S' then
          pbd_mensaje('Valor de venta titulo : '||to_char(ct.vr_venta,'999,999,999,999.99'),pfm_titulo);
          pbd_mensaje('Valor de intereses :    '||to_char(t_descuento,'999,999,999,999.99'),pfm_titulo);
          pbd_mensaje('Valor de venta base  :  '||to_char(glo_vr_venta,'999,999,999,999.99'),pfm_titulo);
       end if;
       for co in c_operaciones loop
         max_va            := max_va+1;
         va_dias  (max_va) := fbd_plazo(co.fecha,pfm_fecha,t_anualidad);
         -- Descontar bisiestos       --jga-11061516-2011/07/27

         if t_anualidad = '365' then  --jga-11061516-2011/07/27
           --va_dias(max_va) := va_dias(max_va) - nvl(fbd_bisiestos(pfm_fecha,co.fecha),0);
           va_dias(max_va) := va_dias(max_va) - nvl(fbd_incluye_bisiestos(pfm_fecha,co.fecha,pfm_titulo),0);  -- 17-02-2012

         end if;
         va_vr_mer(max_va) := co.vr_teo;
         va_ta_mer(max_va) := fn_tasa_referencia(ct.inversion
                                                ,pfm_titulo
                                                ,co.flujo
                                                ,ct.ind_val
                                                ,pfm_fecha
                                                ,ct.moneda_empresa
                                                ,ct.dias_vcto
                                              --,fbd_plazo(co.fecha,pfm_fecha,t_anualidad)  --jga-11061516-2011/07/27
                                                ,va_dias(max_va)                            --jga-11061516-2011/07/27
                                                ,ct.fondo
                                                ,ct.pais_empresa
                                                ,ct.f_compromiso
                                                ,ct.f_compra
                                                ,t_anualidad     -- anualidad portafolio
                                                ,ct.tipo_fijo
                                                ,ct.tasa_fija
                                                );
         if glo_muestre = 'S' then
            pbd_mensaje('Dias: '||to_char(va_dias(max_va),'9999')||' Flujo: '||to_char(va_vr_mer(max_va),'999,999,999,999.99')||' Tasa: '||to_char(va_ta_mer(max_va),'99.9999'),pfm_titulo);
         end if;
       end loop;
     --
     end loop;
     --
   End; -- De pr_cargue_operaciones_venta

   --------------------------------------------------------------------------------
   PROCEDURE pr_cargue_operaciones_mercado is  --jga-11061516-2011/07/27
   --------------------------------------------------------------------------------
   -- Carga las operaciones asociadas al titulo recibido como parametro
   --------------------------------------------------------------------------------
     ------------------------------------------------------------------------------
     -- Es importante destacar que independiente del metodo que se hubiera
     -- tomado para calculo (Futuras Implicias o Proyeccion Simple), los
     -- valores del flujo ya fueron reevaluarlos antes del llamado a esta funcion.
     ------------------------------------------------------------------------------
     cursor c_tasas is
       select inv_consec                      inversion
             ,tit_vppm                        vr_mercado
             ,fbd_desc_especie_inv(inv_consec,'CNV_CODIGO') ind_val
             ,emp_vigila                      vigila
             ,emp_codigo                      empresa
             ,emp_cnv_codigo                  moneda_empresa
             ,emp_pai_codigo                  pais_empresa
             ,por_fondo                       fondo
             ,inv_cin_codigo                  clase
             ,inv_tin_codigo                  tipo
             ,inv_emi_codigo                  emisor
             --,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad) dias_vcto
             ,fbd_plazo(fbd_fecha_redencion(tit_consec),pfm_fecha,t_anualidad)-
              fbd_incluye_bisiestos(pfm_fecha,nvl(tit_f_venta,inv_f_vcto),tit_consec) dias_vcto
             ,inv_f_compromiso                f_compromiso
             ,inv_f_compra                    f_compra
             ,tit_tipo_fijo                   tipo_fijo
             ,tit_efe_dcto_vm                 tasa_fija
             ,inv_f_vigencia                   vencimiento
         from pfempresas_v2
             ,pfportafolios_v2
             ,pfespecies_v2
             ,pfinversiones_v2
             ,pftitulos_v2
        where emp_codigo     = por_emp_codigo
          and por_emp_codigo = inv_emp_codigo
          and por_codigo     = inv_por_codigo
          and esp_cin_codigo = inv_cin_codigo
          and esp_tin_codigo = inv_tin_codigo
          and esp_emi_codigo = inv_emi_codigo
          and inv_consec     = tit_inv_consec
          and tit_consec     = pfm_titulo
      ;
     ----------------------------------------------------------------------
     -- Operaciones asociadas al papel recibido como parametro
     ----------------------------------------------------------------------
      cursor c_operaciones is
        select ope_fecha                   fecha
              ,ope_top_codigo              operacion
              ,decode(nvl(top_signo,'+')
                     ,'+',ope_valor
                         ,-ope_valor
                     ) *
               decode(ope_flujo_teorico
                     ,'S',1
                         ,0
                     )                     vr_flujo
              ,ope_exv_consec              flujo
          from pftipo_operaciones_v2
              ,pfoperaciones_v2
         where top_codigo         = ope_top_codigo
           and ope_flujo_teorico  = 'S'
           and ope_tit_consec     = pfm_titulo
           and ope_fecha         >  pfm_fecha
           and top_aplica_valoracion = 'S'
      ;
     ----------------------------------------------------------------------
     -- Variables locales al procedimiento
     ----------------------------------------------------------------------
     t_ind_genera  pfconversiones_v2.cnv_codigo%type        ;
     t_ind_desta   pfconversiones_v2.cnv_codigo%type        ;
     t_operador    pfvar_inversion_v2.var_operador_desv%type; -- Op. ind. fin.
     t_desviacion  pfvar_inversion_v2.var_desviacion%type   ; -- Desviacion
     t_modalidad   pfmodalidades_v2.mod_modalidad%type      ; -- Modalidad
     t_periodo     pfinversiones_v2.inv_plazo%type          ; -- Periodicidad
     t_incluir     pfoperaciones_v2.ope_incluir_real%type   ;
     t_descuento   pfoperaciones_v2.ope_valor%type          ;
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_operaciones_mercado
     max_va      :=  0;
     t_incluir   := 'N';
     t_descuento :=  0;

     for ct in c_tasas loop -- Para dejar en contexto informacion titulo

       glo_vr_mercado := ct.vr_mercado;

       if glo_muestre = 'S' then
          pbd_mensaje('Valor mercado ant. titulo : '||to_char(ct.vr_mercado,'999,999,999,999.99'),pfm_titulo);
          pbd_mensaje('Valor base calculo margen : '||to_char(glo_vr_mercado,'999,999,999,999.99'),pfm_titulo);
       end if;

       for co in c_operaciones loop

         max_va            := max_va+1;
         va_dias(max_va)   := fbd_plazo(co.fecha,pfm_fecha,t_anualidad);
         -- Descontar bisiestos       --jga-11061516-2011/07/27
         if t_anualidad = '365' then  --jga-11061516-2011/07/27
           --va_dias(max_va) := va_dias(max_va) - nvl(fbd_bisiestos(pfm_fecha,co.fecha),0);
           va_dias(max_va) := va_dias(max_va) - nvl(fbd_incluye_bisiestos(pfm_fecha,co.fecha,pfm_titulo),0);  -- 17-02-2012
         end if;
         va_vr_mer(max_va) := co.vr_flujo;
         va_ta_mer(max_va) := fn_tasa_referencia(ct.inversion
                                                ,pfm_titulo
                                                ,co.flujo
                                                ,ct.ind_val
                                                ,pfm_fecha
                                                ,ct.moneda_empresa
                                                ,ct.dias_vcto
                                              --,fbd_plazo(co.fecha,pfm_fecha,t_anualidad)  --jga-11061516-2011/07/27
                                                ,va_dias(max_va)                            --jga-11061516-2011/07/27
                                                ,ct.fondo
                                                ,ct.pais_empresa
                                                ,ct.f_compromiso
                                                ,ct.f_compra
                                                ,t_anualidad     -- anualidad portafolio
                                                ,ct.tipo_fijo
                                                ,ct.tasa_fija
                                                );
         if glo_muestre = 'S' then
            pbd_mensaje('Dias: '||to_char(va_dias(max_va),'9999')||' Flujo: '||to_char(va_vr_mer(max_va),'999,999,999,999.99')||' Tasa: '||to_char(va_ta_mer(max_va),'99.9999'),pfm_titulo);
         end if;
       end loop;
     end loop;
   End; -- De pr_cargue_operaciones_mercado

   -------------------------------------------------------------------------
   FUNCTION fn_arme_expresion(pfa_valor  number
                             ,pfa_margen number
                             ,pfa_pos    integer
                             ) return number is
   -------------------------------------------------------------------------
   --                                   VFm
   -- Evalua la expresion ------------------------
   --                     [((1+tb1)*(1+m))]^n1/365
   -------------------------------------------------------------------------
     FUNCTION fn_valor return number is
     t_valor number;
     begin
       t_valor := power((1+va_ta_mer(pfa_pos)/100)*(1+pfa_margen/100)
                       ,va_dias(pfa_pos)/t_anualidad
                       );
       t_valor  := pfa_valor / t_valor;
       return(t_valor);
     end;
   -------------------------------------------------------------------------
   Begin -- de fn_arme_expresion
    return(fn_valor);
   End; -- De fn_arme_expresion
   -------------------------------------------------------------------------
   FUNCTION fn_vpn(pfv_margen number) return number is
   -------------------------------------------------------------------------
   -- Calcula el valor presente de las operaciones del cursor con base en el
   -- margen recibido como parametro
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variables locales a la funcion
      ----------------------------------------------------------------------
      t_vpn   number;
      t_tasa  pfinversiones_v2.inv_efe_compra%type;
   -------------------------------------------------------------------------
   Begin -- de fn_vpn
     t_vpn := 0;
     for t_pos in 1 .. max_va loop
      t_vpn := t_vpn + fn_arme_expresion(va_vr_mer(t_pos)
                                        ,pfv_margen
                                        ,t_pos
                                        );
     end loop;
     return(least(t_vpn,999999999999.9999));
   End;
   -------------------------------------------------------------------------
   FUNCTION fn_margen_compra return number is
   -------------------------------------------------------------------------
   -- Calcula la tasa interna de retorno de la ecuacion
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variables locales a la funcion
      ----------------------------------------------------------------------
      t_margen    pfinversiones_v2.inv_efe_compra%type:=0          ; --Tasa prueba
      t_maxima pfinversiones_v2.inv_efe_compra%type:=99.99999999; --Tir Maxima
      t_minima pfinversiones_v2.inv_efe_compra%type:=-99.99999999; --Tir Minima
      t_error  pfinversiones_v2.inv_efe_compra%type:=0.00000001 ; --Error valido
      t_valor  number                              :=1          ;
   -------------------------------------------------------------------------
   Begin -- De fn_margen_compra
     t_margen := 0;
     while abs(t_valor) >= t_error loop
       t_margen   := (t_maxima + t_minima ) / 2;
       t_valor := fn_vpn(t_margen) - glo_vr_compra;

       if t_valor > 0 then t_minima:=t_margen; else t_maxima:=t_margen; end if;
       if (t_maxima-t_minima) <= t_error then t_valor := 0; end if;

     end loop;

     if abs(t_margen) < 0.001 then t_margen := 0; end if;

     if t_margen = 50 then t_margen := 0; end if;

     return(t_margen);
   End; -- De fn_margen_compra

   -------------------------------------------------------------------------
   FUNCTION fn_margen_venta return number is
   -------------------------------------------------------------------------
   -- Calcula la tasa interna de retorno de la ecuacion
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variables locales a la funcion
      ----------------------------------------------------------------------
      t_margen    pfinversiones_v2.inv_efe_compra%type:=0          ; --Tasa prueba
      t_maxima pfinversiones_v2.inv_efe_compra%type:=99.99999999; --Tir Maxima
      t_minima pfinversiones_v2.inv_efe_compra%type:=-99.99999999; --Tir Minima
      t_error  pfinversiones_v2.inv_efe_compra%type:=0.00000001 ; --Error valido
      t_valor  number                              :=1          ;
   -------------------------------------------------------------------------
   Begin -- De fn_margen_venta
     t_margen := 0;
     while abs(t_valor) >= t_error loop
       t_margen   := (t_maxima + t_minima ) / 2;
       t_valor := fn_vpn(t_margen) - glo_vr_venta;
       if t_valor > 0 then t_minima:=t_margen; else t_maxima:=t_margen; end if;
       if (t_maxima-t_minima) <= t_error then t_valor := 0; end if;
     end loop;
     if abs(t_margen) < 0.001 then t_margen := 0; end if;
     if t_margen = 50 then t_margen := 0; end if;
     return(t_margen);
   End; -- De fn_margen_venta

   -------------------------------------------------------------------------
   FUNCTION fn_margen_mercado return number is  --jga-11061516-2011/07/27
   -------------------------------------------------------------------------
   -- Calcula la tasa interna de retorno de la ecuacion
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variables locales a la funcion
      ----------------------------------------------------------------------
      t_margen pfinversiones_v2.inv_efe_compra%type:=0          ;  --Tasa prueba
      t_maxima pfinversiones_v2.inv_efe_compra%type:=99.99999999;  --Tir Maxima
      t_minima pfinversiones_v2.inv_efe_compra%type:=-99.99999999; --Tir Minima
      t_error  pfinversiones_v2.inv_efe_compra%type:=0.00000001 ;  --Error valido
      t_valor  number                              :=1          ;  --Margen calculado
   -------------------------------------------------------------------------
   Begin -- De fn_margen_mercado
     t_margen := 0;

     while abs(t_valor) >= t_error loop

       t_margen   := (t_maxima + t_minima ) / 2;
       t_valor := fn_vpn(t_margen) - glo_vr_mercado;

       if t_valor > 0 then t_minima:=t_margen; else t_maxima:=t_margen; end if;

       if (t_maxima-t_minima) <= t_error then t_valor := 0; end if;

     end loop;

     if abs(t_margen) < 0.001 then t_margen := 0; end if;

     if t_margen = 50 then t_margen := 0; end if;

     return(t_margen);
   End; -- De fn_margen_mercado

----------------------------------------------------------------------------
--  Inicia logica principal de FBD_MARGEN_PROPIO
----------------------------------------------------------------------------
Begin -- De fbd_margen_propio
  glo_margen := 0;

  -- Trae la anualidad del portafolio para los calculos

  select por_anualidad
        ,inv_f_compra
        ,inv_estado
        ,inv_consec
    into t_anualidad
        ,t_f_compra
        ,t_estado
        ,t_inversion
    from pfportafolios_v2
        ,pfinversiones_v2
        ,pftitulos_v2
   where por_codigo     = tit_por_codigo
     and por_emp_codigo = tit_emp_codigo
     and inv_consec     = tit_inv_consec
     and tit_consec     = pfm_titulo;
  -- Salvar estado actual de las operaciones

  max_vao := 0;
  for ico in c_operaciones_todas loop
      max_vao         := max_vao + 1;
      va_ope(max_vao) := ico.ope_consec;
      va_val(max_vao) := ico.ope_valor;
      va_fec(max_vao) := ico.ope_f_recalculo;
  end loop;

  -- Se hace la reevaluacion de los flujos futuros

  -- Reevaluar a fecha de cierre (O compra si no se ha cerrado), por si se ha estado haciendo simulaciones con la fecha de venta.
  if fbd_reevalue(pfm_titulo,t_f_compra,0,t_anualidad) = 1 then null; end if;
  select inv_estado
    into t_estado
    from pfinversiones_v2
   where inv_consec = t_inversion
  ;
  -- Colocar como fecha de compra la de venta, y el estado IPC para que el evaluador funcione OK.
  update pfinversiones_v2
     set inv_f_compra = pfm_fecha
        ,inv_estado   = 'IPC'
   where inv_consec   = t_inversion
  ;

  if fbd_reevalue(pfm_titulo,pfm_fecha,0,t_anualidad) = 1 then null; end if;

  -- Proceso de calculo del margen propio para las compras

  if pfm_tipcal = 'C' then

    -- Carga las operaciones de a procesar por compra en un vector

    pr_cargue_operaciones_compra;

    -- Realiza el calculo del margen de compra

    glo_margen := fn_margen_compra;
    if glo_muestre = 'S' then
       pbd_mensaje('Margen Calculado de Compra: '||to_char(glo_margen,'99.999999999'),pfm_titulo);
    end if;
  -- Proceso para calcular el margen propio de las ventas.

  elsif pfm_tipcal = 'V' then

    -- Carga las operaciones a procesar por venta en un vector

    pr_cargue_operaciones_venta;

    glo_margen := fn_margen_venta;
    if glo_muestre = 'S' then
       pbd_mensaje('Margen Calculado de Venta: '||to_char(glo_margen,'99.999999999'),pfm_titulo);
    end if;

  elsif pfm_tipcal = 'M' then  --jga-11061516-2011/07/27

    -- Carga las operaciones de a procesar por mercado en un vector

    pr_cargue_operaciones_mercado;

    -- Realiza el calculo del margen de mercado

    glo_margen := fn_margen_mercado;
    if glo_muestre = 'S' then
       pbd_mensaje('Margen Calculado de Mercado: '||to_char(glo_margen,'99.999999999'),pfm_titulo);
    end if;


  end if;

    -- Restaurar operaciones
  for i in 1..max_vao loop
      update pfoperaciones_v2
         set ope_valor        = va_val(i)
            ,ope_f_recalculo  = va_fec(i)
       where ope_consec       = va_ope(i)
      ;
  end loop;

  -- Restaurar datos de inversion
  update pfinversiones_v2
     set inv_f_compra = t_f_compra
        ,inv_estado   = t_estado
   where inv_consec   = t_inversion
  ;
  PKG_FLUJOS.Libere_Titulo(PFM_TITULO); --12080823
  return(glo_margen);

end fbd_margen_propio;







/
-------------------------------------------------------------------------------
Show err
