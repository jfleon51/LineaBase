-------------------------------------------------------------------------------
Prompt Compilando fbd_evalue
-------------------------------------------------------------------------------
Create or Replace
function fbd_evalue( pfe_titulo number      -- consec titulo
                                      ,pfe_inversion number   -- Consec inversion
                                      ,pfe_consec_ofl number  -- Consec ofl
                                      ,pfe_fecha_rec date     -- Fecha de recalculo
                                      ,pfe_fecha_oper date    -- Fecha de operacion
                                      ,pfe_ano varchar2       -- Anualidad de evaluacion
                                      ,pfe_tipo varchar2      -- Tipo expresion I, F o V
                                      ,pfe_vna number         -- Valor nominal actual
                                      ,pfe_fecha_proc date    -- Fecha de Proceso
                                      ,pfe_f_desde date
                                      ,pfe_f_hasta date
                                      ,pfe_f_ant_pro_rnp date default null -- Fecha anterior prorroga peri¿dica --jga-08012408-2008/06/26
                                     ) return varchar2
is
  --------------------------------------------------------------------------------------------------
  -- Objetivo: Evalua la expresion recibida como parametro y retorna el resultado.  La evaluacion se
  --           hace segun el tipo de parametro recibido:
  --             I : Sobre la expresion inicial
  --             F : Sobre la expresion final
  --             V : Sobre la expresion valor
  -- Parámetros: pfe_titulo: Pendiente documentar.
  --             pfe_inversion: Pendiente documentar.
  --             pfe_consec_ofl: Pendiente documentar.
  --             pfe_fecha_rec: Pendiente documentar.
  --             pfe_fecha_oper: Pendiente documentar.
  --             pfe_ano: Pendiente documentar.
  --             pfe_tipo: Pendiente documentar.
  --             pfe_vna: Pendiente documentar.
  --             pfe_fecha_proc: Pendiente documentar.
  --             pfe_f_desde: Pendiente documentar.
  --             pfe_f_hasta: Pendiente documentar.
  --             pfe_f_ant_pro_rnp: Pendiente documentar.
  --------------------------------------------------------------------------------------------------
  -- Modificaciones:
  -- CASO     CONSULTOR FECHA      DESCRIPCION CAMBIO
  -- 14121801  Consultor 2014-12-18 Ajuste where
  -- 14122801  JLG       2014-12-19 ajuste2
  --------------------------------------------------------------------------------------------------
  -- DECLARACION DE CURSORES LOCALES A LA FUNCION PORFIN_P.fbd_evalue
  --
  -------------------------------------------------------------------------
  -- Expresion de inicio
  -------------------------------------------------------------------------
  cursor c_inicial is
    select exi_opn_codigo variable
          ,replace(exi_valor,'/','') valor
          ,opn_tipo tipo
      from pfoper_andos_v2
          ,pfexp_inicio_v2
     where opn_codigo     = exi_opn_codigo
       and exi_orden_pf   is not null
       and exi_ofl_consec = pfe_consec_ofl --14121801
     order by exi_orden_pf;
   -------------------------------------------------------------------------
   -- Expresion de finalizacion
   -------------------------------------------------------------------------
   cursor c_final is
      select exf_opn_codigo            variable
            ,replace(exf_valor,'/','') valor
            ,opn_tipo                  tipo
       from pfoper_andos_v2
           ,pfexp_final_v2
      where opn_codigo     = exf_opn_codigo
        and exf_orden_pf   is not null
        and exf_ofl_consec = abs(pfe_consec_ofl)
    order by exf_orden_pf;
   -------------------------------------------------------------------------
   -- Expresion de valor
   -------------------------------------------------------------------------
   cursor c_valor(pcv_f_desde date,pcv_f_hasta date) is
      select decode(exv_opn_codigo
                  ,'CT','TN'
                       ,exv_opn_codigo
                   )         variable
            ,exv_valor       valor
            ,opn_tipo        tipo
            ,decode(opn_clase
                   ,'E01',fbd_dias_reales(nvl(ref_valor,fbd_plazo(decode(pfe_f_ant_pro_rnp,null,inv_f_vcto,pcv_f_hasta),decode(pfe_f_ant_pro_rnp,null,inv_f_emision,pfe_f_ant_pro_rnp),decode(nvl(cin_repo,'N')  --jga-08012408-2008/06/26
                                                                                                 ,'S',por_anualidad
                                                                                                     ,nvl(inv_anualidad_plazo,esp_anualidad_plazo)  -- 10011520
                                                                                                 )
                                                                 )
                                             )
                                         ,pfe_titulo
                                         ,abs(pfe_consec_ofl)
                                         ,pfe_fecha_oper
                                         ,esp_anualidad_flujo
                                         ,pcv_f_desde
                                         ,pcv_f_hasta
                                         ) /
                          (100*to_number(esp_anualidad_flujo_ano)) -- Nominal anual
                   ,'E02',0.01                           -- Porcentual
                   ,'E03',1.0                            -- Fijo
                   ,'E04',1.0                            -- No aplica
                   ,'E05',fbd_dias_reales(nvl(ref_valor,fbd_plazo(decode(pfe_f_ant_pro_rnp,null,inv_f_vcto,pcv_f_hasta),decode(pfe_f_ant_pro_rnp,null,inv_f_emision,pfe_f_ant_pro_rnp),decode(nvl(cin_repo,'N')  --jga-08012408-2008/07/01
                                                                                                 ,'S',por_anualidad
                                                                                                     ,inv_anualidad
                                                                                                 )
                                                                 )
                                             )
                                         ,pfe_titulo
                                         ,abs(pfe_consec_ofl)
                                         ,pfe_fecha_oper
                                         ,esp_anualidad_flujo
                                         ,pcv_f_desde
                                         ,pcv_f_hasta
                                         ) /
                          (100*to_number(esp_anualidad_flujo_ano)) -- Efectiva anual
                   )         factor
            ,decode(opn_clase
                   ,'E01',fbd_dias_reales(nvl(ref_valor,fbd_plazo(decode(pfe_f_ant_pro_rnp,null,inv_f_vcto,pcv_f_hasta),decode(pfe_f_ant_pro_rnp,null,inv_f_emision,pfe_f_ant_pro_rnp),decode(nvl(cin_repo,'N')  --jga-08012408-2008/06/26
                                                                                                 ,'S',por_anualidad
                                                                                                     ,inv_anualidad
                                                                                                 )
                                                                 )
                                             )
                                         ,pfe_titulo
                                         ,abs(pfe_consec_ofl)
                                         ,pfe_fecha_oper
                                         ,esp_anualidad_flujo
                                         ,pcv_f_desde
                                         ,pcv_f_hasta
                                         )
                   ,'E05',fbd_dias_reales(nvl(ref_valor,fbd_plazo(decode(pfe_f_ant_pro_rnp,null,inv_f_vcto,pcv_f_hasta),decode(pfe_f_ant_pro_rnp,null,inv_f_emision,pfe_f_ant_pro_rnp),decode(nvl(cin_repo,'N')  --jga-08012408-2008/07/01
                                                                                                 ,'S',por_anualidad
                                                                                                     ,nvl(inv_anualidad_plazo,esp_anualidad_plazo) -- 10011520
                                                                                                 )
                                                                 )
                                             )
                                         ,pfe_titulo
                                         ,abs(pfe_consec_ofl)
                                         ,pfe_fecha_oper
                                         ,esp_anualidad_flujo
                                         ,pcv_f_desde
                                         ,pcv_f_hasta
                                         )
                         ,1.0
                   )         dias_flujo
            ,opn_clase       clase
    from pfclase_inversiones_v2
        ,pfportafolios_v2
        ,pfreferencias_v2
        ,pfespecies_v2
        ,pfoper_andos_v2
        ,pfexp_valor_v2
        ,pfflujos_inversion_v2
        ,pfinversiones_v2
   where cin_codigo       = inv_cin_codigo
     and por_codigo       = inv_por_codigo
     and por_emp_codigo   = inv_emp_codigo
     and ref_codigo       = fin_mod_periodo
     and esp_cin_codigo   = inv_cin_codigo
     and esp_tin_codigo   = inv_tin_codigo
     and esp_emi_codigo   = inv_emi_codigo
     and opn_codigo       = exv_opn_codigo
     and exv_orden_pf     is not null
     and fin_f_desde      = pcv_f_desde
     and fin_f_hasta      = pcv_f_hasta
     and exv_ofl_consec   = abs(pfe_consec_ofl)
     and fin_inv_consec   = pfe_inversion
     and fin_ofl_consec+0 = abs(pfe_consec_ofl)
     and inv_consec       = pfe_inversion
    order by exv_orden_pf;
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE TIPOS DE VECTORES LOCALES A LA FUNCION fbd_evalue
   --
   -------------------------------------------------------------------------
     type vec_varchar2_1  is table of varchar2(1)  index by binary_integer;
     type vec_varchar2_2  is table of varchar2(2)  index by binary_integer;
     type vec_varchar2_15 is table of varchar2(20) index by binary_integer;
     type vec_varchar2_30 is table of varchar2(33) index by binary_integer;
     type vec_number_6    is table of number(6)    index by binary_integer;

   -------------------------------------------------------------------------
   -- DECLARACION DE VECTORES LOCALES A LA FUNCION fbd_evalue
   -------------------------------------------------------------------------
   -- Vectores para guardar las variables de la inversion
   -------------------------------------------------------------------------
     va_oper   vec_number_6       ; -- Consecutivo Flujo Operacion
     va_var    vec_varchar2_2     ; -- Codigo de la variable
     va_tipo   vec_varchar2_1     ; -- Tipo de la variable
     va_valor  vec_varchar2_15    ; -- Valor de la variable
     va_mod_if vec_varchar2_1     ; -- Modalidad del indicador financiero
     max_va    binary_integer := 0; -- Dimensionamiento de va
   -------------------------------------------------------------------------
   -- Vectores para evaluar expresiones postfija
   -------------------------------------------------------------------------
     ep_tipo  vec_varchar2_1     ; -- Tipo de variable
     ep_valor vec_varchar2_30    ; -- Valor a evaluar
     max_ep   binary_integer := 0; -- Dimensionamiento de ep
     ep_var   vec_varchar2_2     ; -- Codigo de la variable  07020521
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE VARIABLES LOCALES A LA FUNCION fbd_evalue
   --
   -------------------------------------------------------------------------
     t_pos             binary_integer := 0; -- Para recorrer todos los vectores
     t_resultado       varchar2(100)      ; -- Contendra el resultado de la funcion
     glo_metodo        pfreferencias_v2.ref_codigo%type; -- Metodo de valoracion de la especie.
     t_reportada       pftasas_vista_v2.tvi_tasa_reportada%type;
     t_f_desde         date;
     t_f_hasta         date;
     glo_inv_vista     boolean;
     glo_inv_f_emi     date := null;
     glo_f_emision     date := null;
     glo_cliente       pfreferencias_v2.ref_valor%type;
     glo_inv_anualidad pfinversiones_v2.inv_anualidad%type;
     glo_mexico        pfreferencias_v2.ref_valor%type;  --07020521
     glo_tasa_sumarizada  varchar2(1);   -- 07082929
     Salga             Exception;
     VEC_Flu           PKG_EXP.Tipo_Flujo;

   -------------------------------------------------------------------------
   -- DECLARACION DE PROCEDIMIENTOS LOCALES A LA FUNCION fbd_evalue
   -------------------------------------------------------------------------

   -------------------------------------------------------------------------
   PROCEDURE pr_datos_basicos is
   -------------------------------------------------------------------------
   -- Obtiene informaci¿n a ser usada globalmente por la funci¿n
   -------------------------------------------------------------------------
   Begin -- De pr_datos_basicos
     select inv_f_emision
           ,inv_anualidad
       into glo_inv_f_emi
           ,glo_inv_anualidad
       from pfinversiones_v2
      where inv_consec = pfe_inversion
     ;
     glo_cliente   := fbd_cliente;
     glo_f_emision := to_date(fbd_desc_especie_inv(pfe_inversion,'PFEC_EMI'),'YYYYMMDD');
     glo_inv_vista := fbd_inv_vista(pfe_inversion);
     --07020521. Leer la referencia para determinar si aplica a Mejico o no.
     if nvl(ltrim(rtrim(fbd_referencia('DPT','V'))),'N') = 'N'
       then glo_mexico := 'S';
     else glo_mexico := 'S';
     end if;
   End;  -- De pr_datos_basicos

   -------------------------------------------------------------------------
   PROCEDURE pr_cargue_var is
   -------------------------------------------------------------------------
   -- Carga las variables de la inversion en los vectores correspondientes
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Valores de las variables del flujo
      ----------------------------------------------------------------------
      cursor c_variables is
         select ofl_consec                  flujo
               ,decode(var_opn_codigo
                      ,'CT','TN'
                           ,var_opn_codigo
                      )                     variable
               ,decode(var_opn_codigo
                      ,'IF',decode(tit_observacion
                                  ,'__P__',nvl(fbd_desc_especie_inv(inv_consec,'CNV_FUTUROS'),var_valor)
                                          ,var_valor
                                  )
                           ,replace(var_valor,'/','')
                      )                     valor
               ,opn_tipo                    tipo
               ,var_operador_desv           o_if
               ,var_desviacion              desv
               ,var_modalidad_if            mod_if
               ,nvl(var_piso,0)             piso
               ,nvl(var_techo,99.99)        techo
               ,inv_anualidad               anualidad
               ,fbd_desc_especie_inv(inv_consec,'METODO_VALORACION') metodo
       from pfoper_andos_v2
           ,pfoper_flujos_v2
           ,pfvar_inversion_v2
           ,pftitulos_v2
           ,pfinversiones_v2
      where ofl_consec(+)  = var_ofl_consec
        and opn_codigo     = var_opn_codigo
        and var_f_desde    = t_f_desde
        and var_f_hasta    = t_f_hasta
        and var_inv_consec = inv_consec
        and tit_consec     = pfe_titulo
        and inv_consec     = pfe_inversion
      ;
      -- Validar que existe una tasa para inversiones vista
      cursor c_tasa is
         select 'S'              hay_tasa
           from pftasas_vista_v2
          where tvi_tit_consec = pfe_titulo
            and tvi_fecha <= pfe_fecha_proc
      ;
      -------------------------------------------------------------------------
      -- Definici¿n de variables locales al procedimiento pr_cargue_var
      -------------------------------------------------------------------------
        t_formato  pfoper_andos_v2.opn_formato%type;
        t_nominal  number;
        t_codigo   varchar2(5);
        t_operador varchar2(1);
        t_desv     number;
        t_existe   varchar2(1) := 'N';
        t_cau_adicional number;   -- 08010852
      ----------------------------------------------------------------------
      FUNCTION fni_valor(pfv_variable varchar2  -- Codigo de la variable
                        ,pfv_valor    varchar2  -- Valor o cod ind. financiero
                        ,pfv_operador varchar2  -- Operador de desviacion
                        ,pfv_desv     number    -- Desviacion Numerica
                        ,pfv_consec   number    -- Consecutivo Flujo
                        ,pfv_piso     number    -- Piso del indicador
                        ,pfv_techo    number    -- Techo del indicador
                        ,pfv_modalidad varchar2 -- Modalidad Negociacion
                        ,pfv_ano       varchar2 -- Anualidad de Evaluacion
                        ) return varchar2 is
      ----------------------------------------------------------------------
      -- Retorna la conversion del indicador financiero recibiba como
      -- parametro a la periodicidad del flujo, si la variable es un
      -- indicador financiero; de lo contrario retorna el mismo valor
      -- recibido como parametro.
      ----------------------------------------------------------------------
        -- Seleccion de la modalidad del indicador financiero
        cursor c_mod_indicador is
           select nvl(ref_valor,fbd_plazo(t_f_hasta,t_f_desde, pfv_ano)) periodicidad
                 ,cnv_mod_modalidad                                      modalidad
                 ,cnv_tipo                                               cnv_tipo
                 ,cnv_base_efectivizar                                   base_efectiva     -- ORC 20060128
             from pfreferencias_v2
                 ,pfconversiones_v2
            where ref_codigo = cnv_mod_periodo
              and cnv_codigo = pfv_valor
        ;
        -- Seleccion de la modalidad de negociacion del flujo
        cursor c_mod_flujo is
           select nvl(ref_valor,fbd_plazo(inv_f_vcto,inv_f_emision,decode(nvl(cin_repo,'N')
                                                                         ,'S',por_anualidad
                                                                             ,inv_anualidad
                                                                         )
                                         )
                     )                      periodicidad
                 ,fin_mod_modalidad         modalidad
             from pfclase_inversiones_v2
                 ,pfportafolios_v2
                 ,pfreferencias_v2
                 ,pfflujos_inversion_v2
                 ,pfinversiones_v2
            where cin_codigo       = inv_cin_codigo
              and por_codigo       = inv_por_codigo
              and por_emp_codigo   = inv_emp_codigo
              and ref_codigo       = fin_mod_periodo
              and fin_f_desde      = t_f_desde
              and fin_f_hasta      = t_f_hasta
              and fin_ofl_consec+0 = pfv_consec
              and fin_inv_consec   = inv_consec
              and inv_consec       = pfe_inversion
        ;
        cursor c_mod_flujo1 is
           select nvl(ref_valor,fbd_plazo(inv_f_vcto,inv_f_emision,decode(nvl(cin_repo,'N')
                                                                         ,'S',por_anualidad
                                                                             ,inv_anualidad
                                                                         )
                                         )
                     )                      periodicidad
                 ,fin_mod_modalidad         modalidad
             from pfclase_inversiones_v2
                 ,pfportafolios_v2
                 ,pfreferencias_v2
                 ,pfflujos_inversion_v2
                 ,pfinversiones_v2
            where cin_codigo       = inv_cin_codigo
              and por_codigo       = inv_por_codigo
              and por_emp_codigo   = inv_emp_codigo
              and ref_codigo       = fin_mod_periodo
              and fin_f_desde      = t_f_desde
              and fin_f_hasta      = t_f_hasta
              and fin_ofl_consec+0 = pfe_consec_ofl
              and fin_inv_consec   = pfe_inversion
              and inv_consec       = pfe_inversion
        ;
      -------------------------------------------------------------------
      -- Variables locales a la funcion
      -------------------------------------------------------------------
        t_conversion  varchar2(33);
        t_valor       number;
        t_formato     pfoper_andos_v2.opn_formato%type;
        t_dec_tasa    pfparametros_especies_v2.pes_dec_tasa%type := null; -- Formato decimales para tasa
        t_indicador   pfconversiones_v2.cnv_codigo%type;
        t_desv_spread number;
      ----------------------------------------------------------------------
      Begin -- De fni_valor
        t_desv_spread := pfv_desv;    --- LAA 2006/01/20
        if pfv_variable not in ('IF','TE') then -- No es indicador ni efectiva
          if glo_inv_vista and
             pfv_variable = 'TN' and t_reportada = 'E' then
            -- Traer los decimales asociados al indicador financiero
            t_dec_tasa  := fbd_desc_especie_inv(pfe_inversion,'DEC_TASA');

            if (t_dec_tasa is null or glo_inv_f_emi < glo_f_emision) then
               -- Si el cliente es Bancoldex la tasa se redondea a 2 decimales
               if glo_cliente = 'o16' then   -- JCD 20050620
                  t_formato := '999.99';     -- JCD 20050620
               else
                  select nvl(opn_formato,'99.99')
                    into t_formato
                    from pfoper_andos_v2
                   where opn_codigo = pfv_variable;
               end if;
            else
              if glo_cliente = 'o16' then
                t_formato := '999.99';
              else
                t_formato := '999.'||substr('99999999999',1,t_dec_tasa);   -- LAA de 8 a 12
              end if;
            end if;
            for cf in c_mod_flujo1 loop
              t_conversion:=to_char(fbd_efe_nom(pfv_valor
                                              ,cf.modalidad
                                              ,fbd_dias_reales(cf.periodicidad
                                                              ,pfe_titulo
                                                              ,abs(pfe_consec_ofl)
                                                              ,pfe_fecha_oper
                                                              ,pfv_ano
                                                              ,t_f_desde
                                                              ,t_f_hasta
                                                              )
                                              ,pfv_ano
                                              )
                                  ,t_formato
                                  ); -- Formato parametrico
            end loop;
          else  -- No vista
            t_conversion := ltrim(pfv_valor);
          end if;
        else -- Se trata de un indicador financiero o tasa efectiva
          if pfe_tipo != 'V' then t_conversion := 0; -- Solo exp valor tienen valor
          else
            -- Traer los decimales asociados al indicador financiero
            t_dec_tasa  := fbd_desc_especie_inv(pfe_inversion,'DEC_TASA');
            if (t_dec_tasa is null or glo_inv_f_emi < glo_f_emision) then
               -- Si el cliente es Bancoldex la tasa se redondea a 2 decimales
               if glo_cliente = 'o16' then   -- JCD 20050620
                  t_formato := '999.99';     -- JCD 20050620
               else
                  select nvl(opn_formato,'99.99')
                    into t_formato
                    from pfoper_andos_v2
                   where opn_codigo = pfv_variable;
               end if;
               if not glo_inv_vista then t_formato := '999.99999999999'; end if;   -- LAA de 8 a 12
            else
              if glo_cliente = 'o16' then
                t_formato := '999.99';
              else
                t_formato := '999.'||substr('99999999999',1,t_dec_tasa);   -- LAA de 8 a 12
              end if;
            end if;
            if pfv_variable = 'TE' then -- Tratamiento a tasas efectivas
              if ((glo_inv_vista and t_reportada = 'E') or not (glo_inv_vista)) then
                for cf in c_mod_flujo loop
                  t_conversion:=to_char(fbd_efe_nom(pfv_valor
                                                   ,cf.modalidad
                                                   ,fbd_dias_reales(cf.periodicidad
                                                                   ,pfe_titulo
                                                                   ,abs(pfe_consec_ofl)
                                                                   ,pfe_fecha_oper
                                                                   ,pfv_ano
                                                                   ,t_f_desde
                                                                   ,t_f_hasta
                                                                   )
                                                   ,pfv_ano
                                                   )
                                       ,t_formato
                                       ); -- Formato parametrico
                end loop;
              elsif glo_inv_vista and t_reportada = 'N' then
                    t_conversion := pfv_valor;
              end if;
            else -- Tratamiento a indicadores financieros

             -- Por ejemplo, si papel es T.V. pagando DTF+2, donde la DTF se reporr¿ta T.A.:
             -- 1. Buscar valor de la DTF a trav¿s de la funci¿n fbd_tasa_futura la cual devuelve el valor en terminos
             --    efectivos, por tanto es necesario convertirla a nominal en modalidad de la DTF (T.A.)
             -- 2. Sumarle al resultado los 2 puntos
             -- 3. Convertir a el resultado que est¿ en T.A., a T.V., pasando as¿: de T.A. a efectiva y de efectiva a T.V.


             -- 1. Obtener valor del indicador
             for ci in c_mod_indicador loop -- Para saber la modalidad
                if glo_metodo in ('QMI','QTI','QIS') then   -- ORC 20030425
                  t_valor := fbd_efe_nom(fbd_tasa_futura(pfe_titulo     -- Titulo
                                                        ,pfv_valor      -- Indica
                                                        ,pfe_fecha_proc -- Proc
                                                        ,pfe_fecha_oper -- Oper
                                                        ,pfe_fecha_rec  -- Recal
                                                        ,pfv_modalidad  -- ModNeg
                                                        ,nvl(ci.base_efectiva,pfe_ano)     -- ORC 20060128. Estaba pfe_ano
                                                        ,abs(pfe_consec_ofl) -- Generador flujo
                                                        ,t_f_desde
                                                        ,t_f_hasta
                                                        )
                                        ,ci.modalidad
                                        ,ci.periodicidad
                                        ,nvl(ci.base_efectiva,pfv_ano)  -- ORC 20060128. Estaba pfv_ano
                                        );
                else
                  -- Si es una operaci¿n vista y el tipo de indicador es de cuenta de ahorro se busca el
                  -- c¿digo del indicador asociado(hijo) al indicador del flujo y con base en el saldo nominal actual de la cuenta
                  if glo_inv_vista  and (ci.cnv_tipo = 'WIC') then t_indicador := fbd_buscar_indicador(pfv_valor, pfe_vna);
                  else                                             t_indicador := pfv_valor;
                  end if;
                  if fbd_sumariza(t_indicador) then glo_tasa_sumarizada := 'S'; end if;   -- 07082929
                  t_valor := fbd_efe_nom(fbd_tasa_futura(pfe_titulo                     -- Titulo
                                                        ,t_indicador                    -- Indica
                                                        ,pfe_fecha_proc                 -- Proc
                                                        ,pfe_fecha_oper                 -- Oper
                                                        ,pfe_fecha_rec                  -- Recal
                                                        ,pfv_modalidad                  -- ModNeg
                                                        ,nvl(ci.base_efectiva,pfv_ano)  -- Base Conversion -- ORC 20060128. Estaba pfv_ano
                                                        ,abs(pfe_consec_ofl)            -- Generador flujo
                                                        ,t_f_desde
                                                        ,t_f_hasta
                                                        )
                                        ,ci.modalidad
                                        ,ci.periodicidad
                                        ,nvl(ci.base_efectiva,pfv_ano)                           -- ORC 2000128. Estaba pfv_ano
                                        );

                  -- Cuando el indicador es periodico se debe devolver a efectiva para que el computo de los puntos los realice en terminos efectivos LAA 2003-11-13
                  -- el computo de los puntos los realice en terminos efectivos LAA 2003-11-13
          -- Se eleva el spread al exponente (365/365.25), para ajustarlo a lo definido en la norma.
                  if fbd_periodico(t_indicador) then                                              -- LAA 2003/11/13
                      t_valor := fbd_nom_efe(t_valor,ci.modalidad,ci.periodicidad,pfv_ano);       -- LAA 2003/11/13
                      t_desv_spread := power((1 + t_desv_spread/100),(365/365.25)) - 1;           -- LAA 2006/01/20
              t_desv_spread := t_desv_spread * 100;                                       -- LAA 2006/01/20
                  end if;                                                                         -- LAA 2003/11/13
                end if;

             -- 2. Sumar los puntos
             -- Se realiza el cambio de variable pfv_spread por t_desv_spread, para poder realizar la asignacion
             -- del nuevo valor calculado cuando se trata de un bono pensional.                   -- LAA 2006/01/20
               if fbd_capitaliza(pfv_valor) or fbd_indexado(pfv_valor) or fbd_periodico(pfv_valor) then -- LAA 2003/11/13
                  if    pfv_operador = '+' then t_valor:=(t_valor+t_desv_spread)+(t_valor*t_desv_spread/100);
                  elsif pfv_operador = '-' then t_valor:=(t_valor-t_desv_spread)-(t_valor*t_desv_spread/100);
                  elsif pfv_operador = '*' then t_valor:=(t_valor*t_desv_spread);
                  elsif pfv_operador = '/' then t_valor:=(t_valor/t_desv_spread);
                  end if;
                elsif pfv_operador = '+' then t_valor := t_valor + t_desv_spread;
                elsif pfv_operador = '-' then t_valor := t_valor - t_desv_spread;
                elsif pfv_operador = '*' then t_valor := t_valor * t_desv_spread;
                elsif pfv_operador = '#' then t_valor := t_valor / t_desv_spread;
                end if;
                -- cuando es peridico, debe convertirse a nominal   LAA 2003-11-13
                if fbd_periodico(t_indicador) then                                             -- LAA 2003/11/13
                   t_valor := fbd_efe_nom(t_valor,ci.modalidad,ci.periodicidad,pfv_ano);       -- LAA 2003/11/13
                end if;
              end loop;


             -- 3. Convertir a la modalidad de la inversi¿n
              for ci in c_mod_indicador loop
                for cf in c_mod_flujo loop
                  if glo_cliente = 'o16' then
                    t_valor := round(t_valor,2);
                  end if;
                  -- 07082929. La conversion se realiza a la modalidad de la inversion, lo que cambia el valor de la tasa
                  --           en el caso de tasa sumarizada, debe dejar la tasa sin esta conversion.
                  if glo_tasa_sumarizada = 'S' then                  --  07082929
                    t_conversion := to_char(t_valor/100,t_formato);  --  07082929
                  else                                               --  07082929
                    t_conversion:=to_char(fbd_efe_nom(fbd_nom_efe(t_valor
                                                                 ,ci.modalidad
                                                                 ,ci.periodicidad
                                                                 ,nvl(ci.base_efectiva,pfv_ano)   -- ORC 20060128 Estaba pfv_ano
                                                                 )
                                                     ,cf.modalidad
                                                     ,fbd_dias_reales(cf.periodicidad
                                                                     ,pfe_titulo
                                                                     ,abs(pfe_consec_ofl)
                                                                     ,pfe_fecha_oper
                                                                     ,pfv_ano
                                                                     ,t_f_desde
                                                                     ,t_f_hasta
                                                                     )
                                                     ,pfv_ano
                                                     )
                                         ,t_formato
                                         ); -- Formato parametrico
                  end if;                                            -- 07082929
                end loop;
              end loop;

              -- Si se pasa de los limites definidos se ajusta a los mismos.  LAR 2003.07.09 No aplica para inversiones vista
              if not glo_inv_vista then
                if    to_number(t_conversion)<pfv_piso  then t_conversion := to_char(pfv_piso ,'99.99');
                elsif to_number(t_conversion)>pfv_techo then t_conversion := to_char(pfv_techo,'99.99');
                end if;
              end if;
            end if; -- De si 'TE'
          end if; -- De si = 'V'
        end if;   -- De si = 'IF'
        return (t_conversion);

      End; -- De fni_valor
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_var
     glo_tasa_sumarizada := 'N';  -- 07082929
     for cv in c_variables loop
       glo_metodo := cv.metodo;
       -- Verifica si se trata de un indicador financiero del proximo flujo
       if cv.variable = 'IF' and pfe_consec_ofl < 0 then
          if fbd_desc_especie_inv(pfe_inversion,'CNV_ALTERNO') is not null then
            cv.valor := fbd_desc_especie_inv(pfe_inversion,'CNV_ALTERNO');
          end if;
       end if;
       if    cv.variable = 'TN' then t_reportada := 'N';
       elsif cv.variable = 'TE' then t_reportada := 'E';
       else                          t_reportada := null;
       end if;
       if glo_inv_vista then
          t_existe := 'N';
          for ict in c_tasa loop t_existe := ict.hay_tasa; end loop;
          if t_existe = 'S' then
            if cv.variable in ('TN') then
              t_nominal := fbd_tasa_actual(pfe_titulo
                                           ,pfe_fecha_proc
                                           ,'N'
                                           ,t_codigo
                                           ,t_operador
                                           ,t_desv
                                           ,t_reportada);
              cv.valor := to_char(t_nominal);
            elsif cv.variable in ('TE') then
              t_nominal := fbd_tasa_actual(pfe_titulo
                                           ,pfe_fecha_proc
                                           ,'E'
                                           ,t_codigo
                                           ,t_operador
                                           ,t_desv
                                           ,t_reportada);
              cv.valor := to_char(t_nominal);
            -- Inicio Caso 00121825
            elsif cv.variable = 'IR' then
              cv.valor:= nvl(fbd_tasa_actual(pfe_titulo
                                            ,pfe_fecha_proc
                                            ,'V'
                                            ,t_codigo
                                            ,t_operador
                                            ,t_desv
                                            ,t_reportada
                                            ),cv.valor);
            -- Fin Caso 00121825
            end if;
          end if;
       end if;
       if cv.variable = 'KC' then
          cv.valor := fbd_capital_corregido(pfe_titulo
                                           ,pfe_fecha_oper
                                           ,cv.valor
                                           ,pfe_consec_ofl
                                           );
       end if;
       /* Inicio 08010852 */
       if cv.variable = 'VK' then
         t_cau_adicional := 0;
         cv.valor := to_char(fbd_capital_mes(pfe_titulo,pfe_fecha_oper,'S',t_cau_adicional),'99999999999.9999');
       end if;
       /* Fin 08010852 */
       max_va := max_va + 1;

       va_oper  (max_va) := cv.flujo;
       va_var   (max_va) := cv.variable;
       va_tipo  (max_va) := cv.tipo;
       va_mod_if(max_va) := cv.mod_if;
       va_valor (max_va) := fni_valor(cv.variable,cv.valor,cv.o_if,cv.desv
                                     ,cv.flujo,cv.piso,cv.techo,cv.mod_if
                                     ,cv.anualidad
                                     );
     end loop;
   end pr_cargue_var;
   -------------------------------------------------------------------------
   FUNCTION fn_evalue return varchar2 is
   -------------------------------------------------------------------------
   -- Evalua la expresion postfija del vector y retorna su evaluacion
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Declaracion de la pila de evaluacion
      ----------------------------------------------------------------------
        pi_tipo  vec_varchar2_1     ;
        pi_valor vec_varchar2_30    ;
        t_aux    binary_integer := 0;
        pi_var   vec_varchar2_2     ;   --07020521
      ----------------------------------------------------------------------
      PROCEDURE pri_empile(pp_valor varchar2,pp_tipo varchar2,pp_var varchar2) is
      ----------------------------------------------------------------------
      -- Mete en la pila el valor recibido como parametro y su tipo.
      ----------------------------------------------------------------------
      Begin
        t_aux := t_aux + 1;
        pi_tipo (t_aux) := pp_tipo;
        pi_valor(t_aux) := pp_valor;
        pi_var  (t_aux) := pp_var;  --07020521
      End;
      ----------------------------------------------------------------------
      PROCEDURE pri_desempile(pf_operador varchar2) is
      ----------------------------------------------------------------------
      -- Opera los dos valores que se encuentren en el tope de la pila,
      -- los desempila y deja el resultado en el tope de la pila
      ----------------------------------------------------------------------
     -------------------------------------------------------------------
     -- Declaracion de variables locales
     -------------------------------------------------------------------
       t_fecha     date   := null;
       t_fecha1    date   := null;
       t_fecha2    date   := null;
       t_numero    number := null;
       t_numero1   number := null;
       t_numero2   number := null;
       t_anualidad pfinversiones_v2.inv_anualidad%type;                      -- ORC 20041203
       t_dec_fac pfparametros_especies_v2.pes_dec_factor%type := null; -- Formato para factor
       t_f_emision date:=null;
       t_inv_f_emi date:=null;
       t_var1      pfvar_inversion_v2.var_opn_codigo%type       := null; -- 07020521.
       t_var2      pfvar_inversion_v2.var_opn_codigo%type       := null; -- 07020521.
      ----------------------------------------------------------------------
      Begin -- De pri_desempile
        -- Determinar anualidad de la inversion para evaluaci¿n de fechas.
        t_anualidad := pfe_ano;
        -- Cuando el parametro es expresi¿n valor, se tiene en cuenta la anualidad de la inversi¿n.
        if pfe_tipo = 'V' then
          t_anualidad := glo_inv_anualidad;
        end if;
        t_var1 := pi_var(t_aux-1);  --07020521
        if pi_tipo(t_aux-1) = 'F' then t_fecha1  := to_date(pi_valor(t_aux-1),'YYYYMMDD');
        else                           t_numero1 := to_number(pi_valor(t_aux-1));
        end if;

        t_var2 := pi_var(t_aux);    --07020521

        if pi_tipo(t_aux) = 'F' then t_fecha2  := to_date(pi_valor(t_aux),'YYYYMMDD');
        else                         t_numero2 := to_number(pi_valor(t_aux));
        end if;

        if    t_fecha1 is not null and t_fecha2 is not null then
          -- Cuando ambas son fechas solo se permite la diferencia
          t_numero := fbd_plazo(t_fecha1,t_fecha2,t_anualidad);
        elsif t_fecha1 is not null then -- expresion de fechas
          --if mod(t_numero2,30) <> 0 then t_anualidad := 365; end if; -- RRC 2006/05/26 Constantes numericas no multiplos de 30 siempre se calculan base 365
      --07020521.
      if glo_mexico = 'S' then
        if mod(t_numero2,30) <> 0 then t_anualidad := 365; end if;
      else
        if mod(t_numero2,30) <> 0 and t_var2 = 'CN' then t_anualidad := 365; end if;
      end if;
      --Fin 07020521
          if    pf_operador = '+' then t_fecha:=fbd_vencimiento( t_numero2,t_fecha1,t_anualidad);
          elsif pf_operador = '-' then t_fecha:=fbd_vencimiento(-t_numero2,t_fecha1,t_anualidad);
          end if;
        elsif t_fecha2 is not null then -- expresion de fechas
          --if mod(t_numero1,30) <> 0 then t_anualidad := 365; end if; -- RRC 2006/05/26 Constantes numericas no multiplos de 30 siempre se calculan base 365
      --07020521.
      if glo_mexico = 'S' then
        if mod(t_numero1,30) <> 0 then t_anualidad := 365; end if;
      else
        if mod(t_numero1,30) <> 0 and t_var1 = 'CN' then t_anualidad := 365; end if;
      end if;
      --Fin 07020521
          if    pf_operador = '+' then t_fecha:=fbd_vencimiento( t_numero1,t_fecha2,t_anualidad);
          elsif pf_operador = '-' then t_fecha:=fbd_vencimiento(-t_numero1,t_fecha2,t_anualidad);
          end if;
        else -- expresion numerica
          if    pf_operador = '+' then t_numero:=t_numero1+t_numero2;
          elsif pf_operador = '-' then t_numero:=t_numero1-t_numero2;
          elsif pf_operador = '*' then t_numero:=t_numero1*t_numero2;
          elsif pf_operador = '#' then t_numero:=t_numero1/t_numero2;
          elsif pf_operador = '^' then t_numero:=power(t_numero1,t_numero2);
          elsif pf_operador = '&' then t_numero:=mod(t_numero1,t_numero2);
          elsif pf_operador = '!' then t_numero:=round(t_numero1,t_numero2);
          elsif pf_operador = '¿' then if t_numero2 <> 0 then t_numero2 := -t_numero2; end if;
                                       t_numero:=round(t_numero1,t_numero2);
          elsif pf_operador = 'Q' then
            t_inv_f_emi  := glo_inv_f_emi;
            t_dec_fac    := fbd_desc_especie_inv(pfe_inversion,'DEC_FACTOR');
            t_f_emision  := to_date(fbd_desc_especie_inv(pfe_inversion,'PFEC_EMI'),'YYYYMMDD');
            if t_dec_fac is not null and t_inv_f_emi >= t_f_emision then t_numero :=round(t_numero1,t_dec_fac);
            else                                                         t_numero :=round(t_numero1,t_numero2);
            end if;
          end if;
        end if;
        t_aux := t_aux - 1;

        if t_fecha is not null then -- Resultado es una fecha
          pi_valor(t_aux) := to_char(t_fecha,'YYYYMMDD');
          pi_tipo(t_aux) := 'F';
        else -- Resultado es un numero
          pi_valor(t_aux) := to_char(t_numero,'999999999999999.999999999999999');
          pi_tipo(t_aux) := 'N';
        end if;
      End; -- De pri_desempile
   -------------------------------------------------------------------------
   Begin -- De fn_evalue
     for t_pos in 1..max_ep loop
       if ep_tipo(t_pos) = 'O' then -- Operador --> evaluar expresion
         pri_desempile(ep_valor(t_pos));
       else -- Se trata de un valor, por lo tanto hay que empilar.
         pri_empile(ep_valor(t_pos),ep_tipo(t_pos),ep_var(t_pos));  --07020521
       end if;
     end loop;
     return(pi_valor(t_aux));
   end fn_evalue; -- De
   -------------------------------------------------------------------------
   PROCEDURE pr_cargue_postfija(ppc_tipo   varchar2 -- tipo de operando
                               ,ppc_var    varchar2 -- codigo de la variable
                               ,ppc_valor  varchar2 -- Valor de la variable
                               ,ppc_factor number   -- factor de multiplicacion
                               ,ppc_dias_flujo number
                               ) is
   -------------------------------------------------------------------------
   -- Carga los valores de las expresiones postfija recibida como parametro
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      FUNCTION fni_valor_var return varchar2 is
      ----------------------------------------------------------------------
      -- Retorna el valor de la variable a partir de los vectores de almace-
      -- namiento o de los parametros generales de la funcion si estos no son
      -- nulos.
      -- de recalculo del indicador financiero, si aplica.
      -------------------------------------------------------------------------
     ----------------------------------------------------------------------
     -- Declaracion de variables locales
     ----------------------------------------------------------------------
         t_valor   varchar2(33) := null;
         t_tipo    pfreferencias_v2.ref_codigo%type;
         t_formato pfoper_andos_v2.opn_formato%type;
         t_dec_fac pfparametros_especies_v2.pes_dec_factor%type := null; -- Formato para factor
         t_calculo number(30,14):=0.00;
         t_f_inicial   date;              -- Fecha de inicio del flujo           --GPQ
         t_factor      number;            -- Factor de liquidaci¿n               --GPQ
         t_anualidad   number (3,0);      -- Anualidad de la captaci¿n 360 o 365 --GPQ
         t_por_codigo  varchar2(2);       -- C¿digo de portafolio                --GPQ
      -------------------------------------------------------------------------
      Begin -- De fni_valor_var
    for t_pos in 1..max_va loop
        if glo_cliente = 'o16' then          -- JCD 20050621 Se deja el mismo c¿digo generado por Bancoldex
           if va_var(t_pos) = ppc_var then
              select opn_clase
                    ,nvl(opn_formato,'999.99')
                into t_tipo
                    ,t_formato
                from pfoper_andos_v2
               where opn_codigo = va_var(t_pos)
              ;
              if va_tipo(t_pos) = 'F' then -- Fechas no tienen conversion
                 t_valor := va_valor(t_pos);
              elsif va_var(t_pos) = 'VA' then -- Valor nominal actual
                 if t_tipo = 'E01' and not fbd_inv_vista(pfe_inversion) then
                    t_valor := to_char(pfe_vna*ppc_factor,'999999999999'||substr(t_formato,instr(t_formato,'.')));
                 else
                    t_valor := to_char(pfe_vna*ppc_factor,'999999999999.999999999999999');
                 end if;
              else
                 if t_tipo = 'E01' and not fbd_inv_vista(pfe_inversion) then
                    -- Obtiene la fecha de emisi¿n para determinar con que formula calcula el flujo  GPQ
                    select inv_f_vcto
                          ,inv_anualidad
                          ,inv_por_codigo
                      into t_f_inicial
                          ,t_anualidad
                          ,t_por_codigo
                      from pfinversiones_v2
                     where inv_consec = pfe_inversion;
                    -- Aplica las resoluciones 514 de 2002 y 274 de 2004, si la fecha de emision es menor a 20021001
                    --   el flujo se calcula con la formula de tasa, si la fecha es mayor o igual a 20021001 pero menor
                    --   a 20040601 el flujo se calcula con formula de factor a 4 decimales, sino el flujo de intereses
                    --   se calcula con la formula de tasa
                    --   va_valor(t_pos) tiene el valor de la tasa nominal redondeada a 2 decimales
                    --   ppc_factor tiene el resultado de dividir los dias del flujo en la base de la inversion(360 o 365)
                    t_factor := ppc_factor;
                    if (glo_inv_f_emi > to_date('20020930','yyyymmdd')) and (t_por_codigo <> 'B2') then    --GPQ 200406
                       if to_char(pfe_fecha_oper,'DD') <> to_char(glo_inv_f_emi,'DD') then                 --GPQ 200406
                          select max(ope_fecha)
                            into t_f_inicial
                            from pfoperaciones_v2
                           where ope_tit_consec = pfe_titulo
                             and ope_fecha      < pfe_fecha_oper
                             and ope_top_codigo = '4'
                          ;
                          if t_f_inicial is null then
                             t_f_inicial := glo_inv_f_emi;
                          end if;
                          t_factor := (fbd_plazo(pfe_fecha_oper,t_f_inicial,t_anualidad)/t_anualidad)/100; --GPQ 200406
                       end if;
                    end if;
                    if  glo_inv_f_emi > to_date('20040531','yyyymmdd') then                                --GPQ 200406
                        t_valor := to_char(round(to_number(va_valor(t_pos))* t_factor,6),'99.999999');     --GPQ 200406
                    else
                        if (glo_inv_f_emi > to_date('20020930','yyyymmdd') and t_por_codigo <> 'B2') then  --GPQ 200406
                           t_valor := to_char(round(to_number(va_valor(t_pos))* t_factor,4),'99.9999');    --GPQ 200406
                        else
                           t_valor := to_char(to_number(va_valor(t_pos))*t_factor,'99.999999999');         --GPQ 200406
                        end If;
                    end If;
                 else
                    t_valor := to_char(to_number(va_valor(t_pos))*ppc_factor,'999999999999.999999999999999');
                 end if;
              end if;
              if va_oper(t_pos) = abs(pfe_consec_ofl) then exit; end if;
           end if;
        else
          if va_var(t_pos) = ppc_var and ( (va_oper(t_pos) = pfe_consec_ofl and va_oper(t_pos) <> 0) OR (va_oper(t_pos) = 0))  then
            -- Traer los decimales asociados al factor
            select opn_clase
                  ,nvl(opn_formato,'999.9999')
              into t_tipo
                  ,t_formato
              from pfoper_andos_v2
             where opn_codigo = va_var(t_pos)
            ;
            t_dec_fac := fbd_desc_especie_inv(pfe_inversion,'DEC_FACTOR');

            if t_dec_fac is not null and glo_inv_f_emi >= glo_f_emision then
              t_formato := '999.'||substr('99999999999',1,t_dec_fac);    -- LAA de 8 a 12
            end if;
            if    va_tipo(t_pos) = 'F' then -- Fechas no tienen conversion
                  t_valor := va_valor(t_pos);
            elsif va_var(t_pos) = 'VA' then -- Valor nominal actual
                  if t_tipo = 'E01' and not glo_inv_vista then
                    t_valor := to_char(pfe_vna*ppc_factor,'999999999999999'||substr(t_formato,instr(t_formato,'.')));
                  else
                    t_valor := to_char(pfe_vna*ppc_factor,'999999999999999.999999999999999');
                  end if;
            /* Inicio 08010852 */
            elsif va_var(t_pos) = 'VK' then -- Valor capital mes
                  if t_tipo = 'E01' and not glo_inv_vista then
                    t_valor := to_char(to_number(va_valor(t_pos))*ppc_factor,'999999999999999'||substr(t_formato,instr(t_formato,'.')));
                  else
                    t_valor := to_char(to_number(va_valor(t_pos))*ppc_factor,'999999999999999.999999999999999');
                  end if;
            /* Fin 08010852 */
            else  t_calculo:=to_number(va_valor(t_pos))*ppc_factor;
                  if t_tipo = 'E01' and not glo_inv_vista then
                    t_valor := to_char(t_calculo,'999999999999999'||substr(t_formato,instr(t_formato,'.')));
                  else
                    t_valor := to_char(t_calculo,'999999999999999.999999999999999');
                  end if;
            end if;
            if va_oper(t_pos) = abs(pfe_consec_ofl) then exit; end if;
          end if;
        end if;
    end loop;
    return(t_valor);
   End; -- De fni_valor_var
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_postfija
     max_ep := max_ep + 1;
     ep_tipo (max_ep) := ppc_tipo;
     ep_var(max_ep)  := ppc_var;    --07020521
     if    ppc_tipo = 'O'         then -- El valor es el mismo parametro
       ep_valor(max_ep) := ppc_var;
     elsif ppc_var in ('CF','CN') then -- Constante --> trae el valor
       ep_valor(max_ep) := ppc_valor;
     else  ep_valor(max_ep) := fni_valor_var;
     end if;
   End; -- De pr_cargue_postfija

   -------------------------------------------------------------------------
   Function fn_retorne_valor Return Varchar2 Is
      -------------------------------------------------------------------------
      -- Cursor con los datos complementarios del titulo para el proceso
      -------------------------------------------------------------------------
      Cursor C_Datos Is
        Select Decode(ofl_top_codigo
                     ,'Y','CAPITALIZACION'
                     ,'5','REINTEGRO'
                     ,'4','INTERESES'
                     ,'0','EMISION'
                     ,'1','COMPRA'
                     ,'2','VENTA'
                     ,'3','REDENCION'
                     ,'p','INCREMENTO'
                     ,'q','RETIRO'
                     ) operacion
         From pfoper_flujos_v2
        Where ofl_consec = pfe_consec_ofl
      ;
   -------------------------------------------------------------------------
   t_valor Varchar2(100) := '0';
   -------------------------------------------------------------------------
   Begin -- De fn_retorne_valor
     For ICD In C_Datos Loop
       -------------------------------------------------------------------------
       -- Genera los flujos y los carga en el vector VEC_Flu
       -------------------------------------------------------------------------
       PKG_FLUJOS.Flujos_Proyectados(pfe_titulo,pfe_fecha_proc,Null,Null,ICD.Operacion,VEC_Flu);
       -------------------------------------------------------------------------
       -- Devuelve el valor correspondiente que viene en el Vector VEC_Flu
       -------------------------------------------------------------------------
       If VEC_Flu.Count > 0 Then -- Se generaron flujos
         For IVF In VEC_Flu.First .. VEC_Flu.Last Loop
           If VEC_Flu(IVF).Fecha = pfe_fecha_oper And VEC_Flu(IVF).Operacion = icd.operacion Then
              If pfe_tipo = 'V' Then t_valor := To_Char(VEC_Flu(IVF).Valor);
              Else                   t_valor := To_Char(VEC_Flu(IVF).Fecha,'YYYYMMDD');
              End If;
              Exit;
           End If;
         End Loop; -- IVF In VEC_Flu.First .. VEC_Flu.Last
       End If; -- VEC_Flu.Count > 0
     End Loop; --  ICD In C_Datos

     ---------------------------------------- PGR 25-01-2011
     -- Si no encuentra fecha inicial retorna desde y si no encuentra fecha final retorna hasta
     if t_valor = 0 and pfe_tipo = 'I' then
        t_valor := pfe_f_desde;
     else if t_valor = 0 and pfe_tipo = 'F' then
          t_valor := pfe_f_hasta;
          end if;
     end if;
     ---------------------------------------- PGR 25-01-2011
     Return(t_valor);
   End; -- De fn_retorne_valor
   -------------------------------------------------------------------------
----------------------------------------------------------------------------
begin -- De fbd_evalue
  ------------------------------------------------------------------------------------------------------
  -- Verificar si se esta trabajando con el nuevo o con el viejo valorador
  ------------------------------------------------------------------------------------------------------
  if PKG_Val.Metodo(pfe_titulo) = 'N'  Then
    t_resultado := fn_retorne_valor;
    Raise Salga;
  end If;
  ------------------------------------------------------------------------------------------------------
  -- De aqui en adelante todo se comprorta como estaba sin el nuevo valorador
  ------------------------------------------------------------------------------------------------------
  pr_datos_basicos;
  if pfe_f_desde is null or pfe_f_hasta is null then
    pbd_fechas(pfe_titulo,'U',null,t_f_desde,t_f_hasta);
  else
    t_f_desde := pfe_f_desde;
    t_f_hasta := pfe_f_hasta;
  end if;
  pr_cargue_var; -- Carga las variables asociadas a la inversion
  if    pfe_tipo = 'I' then -- Se trata de la expresion inicial
       for ci in c_inicial loop
         pr_cargue_postfija(ci.tipo,ci.variable,ci.valor,1,1);
       end loop;
  elsif pfe_tipo = 'F' then -- Se trata de la expresion final
       for cf in c_final loop
         pr_cargue_postfija(cf.tipo,cf.variable,cf.valor,1,1);
       end loop;
  elsif pfe_tipo = 'V' then -- Se trata de la expresion de valor
         for cv in c_valor(t_f_desde,t_f_hasta) loop
         if glo_tasa_sumarizada = 'S' then cv.factor := 1; end if; -- 07082929
         pr_cargue_postfija(cv.tipo,cv.variable,cv.valor,cv.factor,cv.dias_flujo);
       end loop;
  end if;
  t_resultado := fn_evalue;
  return(t_resultado);
exception
  when salga then
    return(t_resultado);
end fbd_evalue;






/
-------------------------------------------------------------------------------
Show err
