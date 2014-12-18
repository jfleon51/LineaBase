-------------------------------------------------------------------------------
Prompt Compilando fbd_flujo
-------------------------------------------------------------------------------
Create or Replace
function fbd_flujo ( PFF_INVERSION NUMBER
                                      ,PFF_F_DESDE   DATE
                                      ,PFF_F_HASTA   DATE) RETURN NUMBER
is
  ----------------------------------------------------------------------------
  -- Genera las operaciones asociadas al flujo recibido como parametro
  ----------------------------------------------------------------------------
   -------------------------------------------------------------------------
   -- Operaciones asociadas al flujo
   -------------------------------------------------------------------------
   cursor c_operaciones is
      select ofl_consec                            consecutivo
            ,ofl_top_codigo                        operacion
            ,nvl(ref_valor,fbd_plazo(inv_f_vcto,inv_f_emision,decode(nvl(cin_repo,'N')
                                                                    ,'S',por_anualidad
                                                                        ,nvl(inv_anualidad_plazo,esp_anualidad_plazo)  -- 10011520
                                                                    )
                                    )
                )                                  periodo
            ,decode(ref_valor
                   ,null,nvl(inv_anualidad_plazo,esp_anualidad_plazo)  -- 10011520
                        ,'360'
                   )                               anualidad
            ,por_anualidad                         anualidad_por
        from pfclase_inversiones_v2
            ,pfreferencias_v2
            ,pfflujos_inversion_v2
            ,pfoper_flujos_v2
            ,pfportafolios_v2
            ,pfinversiones_v2
            ,pfespecies_v2    -- 10011520
       where cin_codigo     = inv_cin_codigo
         and ref_codigo     = fin_mod_periodo
         and fin_f_desde    = pff_f_desde
         and fin_f_hasta    = pff_f_hasta
         and fin_ofl_consec = ofl_consec
         and fin_inv_consec = inv_consec
         and ofl_flu_codigo = inv_flu_codigo
         and por_emp_codigo = inv_emp_codigo
         and por_codigo     = inv_por_codigo
         and inv_consec     = pff_inversion
         and inv_cin_codigo = esp_cin_codigo  -- 10011520
         and inv_tin_codigo = esp_tin_codigo  -- 10011520
         and inv_emi_codigo = esp_emi_codigo  -- 10011520
    order by ofl_jerarquia;

  cursor c_tipo_periodicidad(pct_flujo number) is
    select var_opn_codigo                        tipo_periodicidad
      from pfvar_inversion_v2
     where var_f_desde+0      = pff_f_desde
       and var_f_hasta+0      = pff_f_hasta
       and var_opn_codigo||'' in ('PE','PU','PP')
       and var_ofl_consec+0   = pct_flujo
       and var_inv_consec     = pff_inversion;
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE TIPOS DE VECTORES LOCALES A LA FUNCION fbd_flujo
   --
   -------------------------------------------------------------------------
   type vec_varchar2_1  is table of varchar2(1)  index by binary_integer;
   type vec_varchar2_6  is table of varchar2(6)  index by binary_integer;
   type vec_date        is table of date         index by binary_integer;
   type vec_number_6    is table of number(6)    index by binary_integer;
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE VECTORES LOCALES A LA FUNCION fbd_flujo
   --
   -------------------------------------------------------------------------
   fl_fecha     vec_date           ; -- Fecha del flujo
   fl_recalc    vec_date           ; -- Fecha de reclaculo del flujo
   fl_modo      vec_varchar2_1     ; -- Modo de reclaculo de fechas (A o V)
   fl_oper      vec_varchar2_6     ; -- Operacion asociada al flujo
   fl_corregido vec_varchar2_1     ; -- Capital Corregido
   fl_exv       vec_number_6       ; -- Expresion valor asociada al flujo
   fl_pais            pfempresas_v2.emp_pai_codigo%type; --Codigo del pais de la empresa
   max_fl       binary_integer := 0; -- Dimensionamiento de fl
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE VARIABLES LOCALES A LA FUNCION fbd_flujo
   --
   -------------------------------------------------------------------------
   -- Variable para recorrer los vectores
   -------------------------------------------------------------------------
   t_pos      binary_integer := 0; -- Para todos los publicos
   -------------------------------------------------------------------------
   -- Variables de trabajo
   -------------------------------------------------------------------------
   f_inicial      date;            -- fecha inicial de flujo
   f_final        date;            -- fecha final de flujo
   f_temp         date;            -- fecha temporal de evaluacion
   f_ant          date;            -- fecha temporal de recalculo
   glo_error      number(1) := 0;  -- Controla si hubo error o no
   glo_empresa    pfportafolios_v2.por_emp_codigo%type; -- Codigo empresa
   glo_portafolio pfportafolios_v2.por_codigo%type;     -- Codigo portafolio
   glo_conversion pfconversiones_v2.cnv_codigo%type;    -- Unidad conversion
   glo_nominal    pfinversiones_v2.inv_vr_nominal%type; -- Valor nominal
   glo_precio     pfinversiones_v2.inv_precio%type;     -- Precio inversion
   glo_titulo     pftitulos_v2.tit_consec%type;         -- Numero del titulo
   glo_ano        pfreferencias_v2.ref_codigo%type;     -- Anualidad de la inversion
   glo_f_emision  date;            -- Fecha Emision del papel
   glo_dia        number(2);       -- Dia de inicio de una fecha
   glo_ft_variable date;           -- Fecha de la tasa variable
   glo_repo       varchar2(1);     -- Inversion es Repo/Interbancario o no.
   glo_primer_flujo date;
   -- MCE Nov 19/98. Variables para controlar no se generen flujos
   -- posteriores al vencimiento
   glo_vcto       pfinversiones_v2.inv_f_vcto%type;
   glo_flujo      pfflujos_v2.flu_codigo%type;
   t_secuencia    number;   -- ORC 20000118
   t_tipo_periodicidad pfvar_inversion_v2.var_opn_codigo%type;
   anualidad_flujo varchar2(5);    --Tipo de Anualidad para Construir las fechas de flujo
   -------------------------------------------------------------------------
   -- Variables requeridas para el manejo del nuevo valorador
   -------------------------------------------------------------------------
   VEC_Flu  PKG_EXP.Tipo_Flujo;
   GLO_Operacion  Varchar2(1) ;
   -------------------------------------------------------------------------
   -------------------------------------------------------------------------
   --
   -- DECLARACION DE PROCEDIMIENTOS LOCALES A LA FUNCION fbd_flujo
   --
   -------------------------------------------------------------------------
   FUNCTION fn_ultimo_habil(pfu_fecha date) return date is
   -------------------------------------------------------------------------
   -- Retorna el ultimo dia habil del mes asociado a pfu_fecha
   -------------------------------------------------------------------------
     -- Definicion de variables locales a fn_ultimo_habil
     -----------------------------------------------------------------------
       t_fecha date;
   begin
     t_fecha := last_day(pfu_fecha);
     while 1 = 1 loop
       if fbd_habil(t_fecha,fl_pais) = t_fecha
         then return t_fecha;
       else
         t_fecha := t_fecha - 1;
       end if;
     end loop;
   end;
   -------------------------------------------------------------------------
   -- Caso 07092702
   FUNCTION fn_primer_habil(pfu_fecha date) return date is
          t_fecha date;
      begin
        t_fecha := pfu_fecha+1;
        while 1 = 1 loop
          if fbd_habil(t_fecha,fl_pais) = t_fecha
            then return t_fecha;
          else
            t_fecha := t_fecha + 1;
          end if;
        end loop;
   end;
   -------------------------------------------------------------------------
   FUNCTION fn_modo(pfm_consec number) return varchar2 is
   -------------------------------------------------------------------------
   -- Retorna el modo de evaluacion del indicador financiero asociado a la
   -- expresion valor de pfm_consec, o nulo si no aplica
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Informacion de la variable de la expresion
      ----------------------------------------------------------------------
      cursor c_modo is
         select var_modalidad_if                  modalidad
           from pfvar_inversion_v2
          where var_opn_codigo = 'IF' -- Indicador Financiero
            and var_f_desde    = pff_f_desde
            and var_f_hasta    = pff_f_hasta
            and var_ofl_consec = pfm_consec
            and var_inv_consec = pff_inversion
      ;
      ----------------------------------------------------------------------
      -- Variable locales a la funcion
      ----------------------------------------------------------------------
      t_modo varchar2(1) := null;
   -------------------------------------------------------------------------
   Begin -- De fn_modo
     for cm in c_modo loop t_modo := cm.modalidad; end loop;
     return(t_modo);
   End; -- De fn_modo
   -------------------------------------------------------------------------
   FUNCTION fn_corregido(pfc_consec number) return varchar2 is
   -------------------------------------------------------------------------
   -- Retorna 'S' si es capital corregido o 'N' si no aplica
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variable locales a la funcion
      ----------------------------------------------------------------------
      t_corregido varchar2(1) := null;
   -------------------------------------------------------------------------
   Begin -- De fn_corregido
     select 'S' into t_corregido
       from pfvar_inversion_v2
      where var_opn_codigo = 'KC' -- Capital corregido
        and var_f_desde    = pff_f_desde
        and var_f_hasta    = pff_f_hasta
        and var_ofl_consec = pfc_consec
        and var_inv_consec = pff_inversion
     ;
     return(t_corregido);
     exception when no_data_found then return('N');
   End; -- De fn_corregido
   -------------------------------------------------------------------------
   PROCEDURE pr_cargue_inv is
   -------------------------------------------------------------------------
   -- Carga los datos generales de la inversion en variables globales
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Informacion basica de la inversion
      ----------------------------------------------------------------------
      cursor c_inversion is
         select inv_emp_codigo                    empresa
               ,inv_por_codigo                    portafolio
               ,inv_cnv_codigo                    conversion
               ,decode(inv_cnv_codigo
                      ,fbd_emp_unidad(inv_emp_codigo),1
                          ,null
                      )                           precio
               ,nvl(pff_f_desde,inv_f_emision)    emision
               ,inv_vr_nominal                    vr_nominal
               ,inv_anualidad                     anualidad
               ,inv_ft_variable                   ft_variable
               ,tit_consec                        titulo
               ,nvl(cin_repo,'N')                 repo
               ,nvl(cin_fondos,'N')               fondo
               ,inv_f_vcto                        f_vcto_inv
               ,inv_flu_codigo                    flujo
           from pfclase_inversiones_v2
               ,pftitulos_v2
               ,pfinversiones_v2
          where cin_codigo     = inv_cin_codigo
            and tit_inv_consec = inv_consec
            and tit_estado     = 'TSI'
            and inv_consec     = pff_inversion
      ;
   -------------------------------------------------------------------------
   Begin -- De pr_cargue_inv
     for ci in c_inversion loop
       glo_empresa     := ci.empresa                       ;
       glo_portafolio  := ci.portafolio                    ;
       glo_conversion  := ci.conversion                    ;
       glo_nominal     := ci.vr_nominal                    ;
       glo_ano         := ci.anualidad                     ;
       glo_titulo      := ci.titulo                        ;
       glo_f_emision   := ci.emision                       ;
       glo_precio      := ci.precio                        ;
       glo_ft_variable := nvl(ci.ft_variable,glo_f_emision);
       glo_repo        := ci.repo                          ;
       glo_flujo       := ci.flujo                         ;
       if ci.fondo = 'S' then glo_repo := 'S'; end if;
       glo_vcto        := ci.f_vcto_inv;
     end loop;
   End; -- De pr_cargue_inv
   -------------------------------------------------------------------------
   PROCEDURE pr_ordene_flujo is
   -------------------------------------------------------------------------
   -- Ordena el flujo de fechas generado basado en el peso y la fecha.
   -- El metodo de ordenamiemto es el tradicional burbuja
   -- Se parte del hecho de que ya viene ordenado por jerarquia
   -------------------------------------------------------------------------
      ----------------------------------------------------------------------
      -- Variables locales al procedimiento
      ----------------------------------------------------------------------
      t_uno         binary_integer := 0;
      t_dos         binary_integer := 0;
      aux_fecha     date;
      aux_recalc    date;
      aux_modo      varchar2(1);
      aux_corregido varchar2(1);
      aux_oper      pftipo_operaciones_v2.top_codigo%type;
      aux_exv       number(6);
   -------------------------------------------------------------------------
   Begin -- De pr_ordene_flujo
     for t_uno in 1..max_fl-1 loop
       for t_dos in t_uno..max_fl loop
         if fl_fecha(t_dos) < fl_fecha(t_uno) then
            aux_fecha           := fl_fecha    (t_uno);
            aux_oper            := fl_oper     (t_uno);
            aux_recalc          := fl_recalc   (t_uno);
            aux_modo            := fl_modo     (t_uno);
            aux_corregido       := fl_corregido(t_uno);
            aux_exv             := fl_exv      (t_uno);
            fl_fecha (t_uno)    := fl_fecha    (t_dos);
            fl_oper  (t_uno)    := fl_oper     (t_dos);
            fl_recalc(t_uno)    := fl_recalc   (t_dos);
            fl_modo  (t_uno)    := fl_modo     (t_dos);
            fl_corregido(t_uno) := fl_corregido(t_dos);
            fl_exv   (t_uno)    := fl_exv      (t_dos);
            fl_fecha (t_dos)    := aux_fecha          ;
            fl_oper  (t_dos)    := aux_oper           ;
            fl_recalc(t_dos)    := aux_recalc         ;
            fl_modo  (t_dos)    := aux_modo           ;
            fl_corregido(t_dos) := aux_corregido      ;
            fl_exv   (t_dos)    := aux_exv            ;
         end if;
        end loop;
     end loop;
     for t_uno in 1..max_fl-1 loop
        glo_primer_flujo := fl_fecha (t_uno);
        exit;
     end loop;
   End; -- De pr_ordene_flujo
   -------------------------------------------------------------------------
   PROCEDURE pr_ajuste_recalculos is
   -------------------------------------------------------------------------
   -- Ajusta las fechas de recalculo de las operaciones con base en el modo
   -- asociado a cada una. Es importante tener en cuenta que para estos casos
   -- no importa la modalidad de la negociacion sino la modalidad de recalculo
   -- del indicador financiero. Es claro que para flujos no dependientes de
   -- indicadores financieros, siempre la modalidad de recalculo estara en nulo.
   -------------------------------------------------------------------------
   Begin -- De pr_ajuste_recalculos
     for t_pos in 1..max_fl loop
       if    fl_modo(t_pos) is null then fl_recalc(t_pos) := null;
       elsif fl_modo(t_pos) = 'V'   then fl_recalc(t_pos) := fl_fecha(t_pos);
       end if;
       -- Correccion para el manejo de los flujos dependientes de indicadores
       -- historicos pero ajustados a la fecha reportada en la inversion
       if fl_recalc(t_pos) is not null and glo_ft_variable is not null then
          if fl_recalc(t_pos) < glo_ft_variable and
             fl_fecha(t_pos)  < glo_ft_variable then
             fl_recalc(t_pos) := glo_ft_variable;
          end if;
       end if;
       if fl_corregido(t_pos)= 'S' then
         fl_recalc(t_pos) := fl_fecha(t_pos);
       end if;
     end loop;
   End; -- De pr_ajuste_recalculos
   -------------------------------------------------------------------------
   PROCEDURE pr_verifique_recalculos is
   -------------------------------------------------------------------------
   -- Verifica que todas las fechas de recalculo correspondan a una fecha
   -- de operacion (la de la misma operacion o la de la anterior).
   -- Con esto se garantiza que flujos irregulares V-I se recalculen en la
   -- fecha del la anterior operacion.
   -------------------------------------------------------------------------
     -- Definicion de variables locales a pr_verifique_recalculos
     -------------------------------------------------------------------------
       t_encontro varchar2(1);
   -------------------------------------------------------------------------
   Begin -- De pr_verifique_recalculos
     for t_pos in 2..max_fl loop
       t_encontro := 'S';
       if fl_recalc(t_pos) is not null and fl_modo(t_pos) = 'A' then
         t_encontro := 'N';
         for t_i in 1..t_pos loop
           if fl_recalc(t_pos) = fl_fecha(t_i) or (fl_recalc(t_pos) = glo_ft_variable and glo_ft_variable is not null) then
             t_encontro := 'S';
             exit;
           end if;
         end loop;
       end if;
       if t_encontro = 'N' then
         fl_recalc(t_pos) := fl_fecha(t_pos-1);
       end if;
     end loop;
   End; -- De pr_verifique_recalculos
   -------------------------------------------------------------------------
   PROCEDURE pr_ajuste_habiles is
   -------------------------------------------------------------------------
   -- Ajusta la fecha del flujo al anterior o sgte dia habil.
   -------------------------------------------------------------------------
     -- Definicion de variables locales a pr_ajuste_habiles
     -------------------------------------------------------------------------
       t_habiles      pfespecies_v2.esp_habiles%type;
       t_modo_habiles pfespecies_v2.esp_modo_habiles%type;
       t_anterior     date;
       t_posterior    date;
   -------------------------------------------------------------------------
   Begin -- De pr_ajuste_habiles
     select nvl(esp_habiles,'N'),nvl(esp_modo_habiles,'P')
       into t_habiles           ,t_modo_habiles
       from pfespecies_v2
           ,pftitulos_v2
      where esp_cin_codigo = tit_cin_codigo
        and esp_tin_codigo = tit_tin_codigo
        and esp_emi_codigo = tit_emi_codigo
        and tit_consec     = glo_titulo
     ;
     if t_habiles <> 'N' then
       for t_pos in 1..max_fl loop
         if (fl_fecha(t_pos) = glo_vcto and t_habiles = 'V') or t_habiles = 'T' then
           if fbd_habil(fl_fecha(t_pos),fl_pais) <> fl_fecha(t_pos) then
             t_anterior  := fbd_habil_atras(fl_fecha(t_pos),fl_pais);
             t_posterior := fbd_habil(fl_fecha(t_pos),fl_pais);
             if    t_modo_habiles = 'A' then fl_fecha(t_pos) := t_anterior;
             elsif t_modo_habiles = 'P' then fl_fecha(t_pos) := t_posterior;
             elsif t_modo_habiles = 'M' then
                   if abs(fl_fecha(t_pos) - t_anterior) <= abs(fl_fecha(t_pos) - t_posterior) then
                         fl_fecha(t_pos) := t_anterior;
                   else  fl_fecha(t_pos) := t_posterior;
                   end if;
             end if;
           end if;
         end if;
       end loop;
     end if;
   End; -- De pr_ajuste_habiles
   -------------------------------------------------------------------------
   PROCEDURE pr_genere_operaciones is
   -------------------------------------------------------------------------
   -- Genera las operaciones asociadas al flujo con base en el vector resultado
   -------------------------------------------------------------------------
   Begin -- De pr_genere_operaciones
     -- Ya no se arranca la secuencia segun posicion del vector
     -- dado que para el caso de prorrogas ya puede haber operaciones
     -- por tanto se busca la maxima y se arranca en la siguiente.
     begin                                           -- ORC 20000118
       select nvl(max(ope_secuencia),2)+1            -- ORC 20000118
         into t_secuencia                            -- ORC 20000118
         from pfoperaciones_v2                       -- ORC 20000118
        where ope_tit_consec = glo_titulo            -- ORC 20000118
          and ope_top_codigo||'' <> '3'              -- ORC 20000118
       ;                                             -- ORC 20000118
       exception when others then t_secuencia := 3; -- ORC 20000118
     end;                                            -- ORC 20000118
     for t_pos in 1..max_fl loop
         pbd_cree_oper (fl_fecha(t_pos)  -- Fecha operacion
                       ,glo_empresa      -- Empresa
                       ,glo_portafolio   -- Portafolio
                       ,fl_oper(t_pos)   -- Operacion
                       ,glo_titulo       -- Consec Titulo
                       ,0                -- Vlr oper
                       ,glo_precio       -- Precio
                       ,null             -- Precio de la unidad (Solo RV)
                       ,glo_conversion   -- Unidad Conver
                       ,'S'              -- Flujo real
                       ,'S'              -- Flujo Teorico
                       ,glo_nominal      -- Nominal Actual
                       ,'OSI'            -- Estado Oper En simulacion
                       ,fl_exv   (t_pos) -- Consec expresion valor
                       ,fl_recalc(t_pos) -- Fecha de recalculo
                       ,null             -- Intermediario
                       ,t_secuencia        -- Consecutivo secuencia
                       );
       t_secuencia := t_secuencia + 1;  -- ORC 20000118
     end loop;
   End; -- De pr_genere_operaciones
   -------------------------------------------------------------------------
   PROCEDURE pr_nuevos_flujos is
   -------------------------------------------------------------------------
   -- Genera los flujos del titulo utulizando el nuevo valorador
   -------------------------------------------------------------------------
     -------------------------------------------------------------------------
     -- Definicion de variables locales a pr_nuevos_flujos
     -------------------------------------------------------------------------
       t_ofl_consec      pfoper_flujos_v2.ofl_consec%type;

   Begin -- De pr_nuevos_flujos
    -- Guardar el rango del flujo a definir.
        insert into pfrangos_flujos_v2
            (rfl_inv_consec,rfl_flu_codigo,rfl_f_desde,rfl_f_hasta)
        values
            (pff_inversion,glo_flujo,pff_f_desde,pff_f_hasta)
            ;
     -------------------------------------------------------------------------
     -- Genera los flujos y los carga en el vector VEC_Flu
     -------------------------------------------------------------------------
     PKG_Flujos.Libere_Titulo(GLO_Titulo);
     PKG_FLUJOS.Flujos_Proyectados(GLO_Titulo,pff_f_desde,Null,Null,'TODOS',VEC_Flu);
     -------------------------------------------------------------------------
     -- Busca la maxima secuencia generada para el titulo, si ya existia
     -------------------------------------------------------------------------
     Begin
       Select Nvl(Max(ope_secuencia),2)+1 Into t_secuencia
         From pfoperaciones_v2
        Where ope_tit_consec = GLO_Titulo
          And ope_top_codigo||'' <> '3'
       ;
       Exception when others then t_secuencia := 3;
     End;
     -------------------------------------------------------------------------
     -- Crea las operaciones asociadas
     -------------------------------------------------------------------------
     If VEC_Flu.Count > 0 Then -- Se generaron flujos
       For IVF In VEC_Flu.First .. VEC_Flu.Last Loop
         Case VEC_Flu(IVF).Operacion
           When 'CAPITALIZACION' Then GLO_Operacion := 'Y';
           When 'REINTEGRO'      Then GLO_Operacion := '5';
           When 'INTERESES'      Then GLO_Operacion := '4';
           When 'EMISION'        Then GLO_Operacion := '0';
           When 'COMPRA'         Then GLO_Operacion := '1';
           When 'VENTA'          Then GLO_Operacion := '2';
           When 'REDENCION'      Then GLO_Operacion := '3';
           When 'INCREMENTO'     Then GLO_Operacion := 'p';
           When 'RETIRO'         Then GLO_Operacion := 'q';
         End Case;
         If GLO_Operacion Not In ('0','1','2','3') Then
           -- Deducir ofl_consec con base en tipo de operaci¿n y posicion
           Begin
             Select ofl_consec
               into t_ofl_consec
               from pfoper_flujos_v2
              where to_number(substr(Trim(ofl_expresion),1,3)) = VEC_Flu(IVF).Posicion
                and ofl_top_codigo                             = GLO_Operacion
                and ofl_flu_codigo                             = GLO_Flujo
             ;
             exception when others then t_ofl_consec := VEC_Flu(IVF).Posicion;
           End;
           pbd_cree_oper(VEC_Flu(IVF).Fecha     -- Fecha operacion
                        ,GLO_Empresa            -- Empresa
                        ,GLO_Portafolio         -- Portafolio
                        ,GLO_Operacion          -- Operacion
                        ,GLO_Titulo             -- Consec Titulo
                        ,VEC_Flu(IVF).Valor     -- Vlr oper
                        ,GLO_Precio             -- Precio
                        ,Null                   -- Precio de la unidad (Solo RV)
                        ,GLO_Conversion         -- Unidad Conver
                        ,'S'                    -- Flujo real
                        ,'S'                    -- Flujo Teorico
                        ,VEC_Flu(IVF).Nominal   -- Nominal Actual
                        ,'OSI'                  -- Estado Oper En simulacion
                        ,t_ofl_consec           -- Consec expresion valor
                        ,VEC_Flu(IVF).Recalculo -- Fecha de recalculo
                        ,null                   -- Intermediario
                        ,t_secuencia            -- Consecutivo secuencia
                        );
          t_secuencia := t_secuencia + 1;
         End If;
       End Loop;
     End If;
   End;  -- De pr_nuevos_flujos
----------------------------------------------------------------------------
Begin -- De fbd_flujo
  pr_cargue_inv; -- Carga la informacion basica necesaria de la inversion
  ------------------------------------------------------------------------------------------------------
  -- Verificar si se esta trabajando con el nuevo o con el viejo valorador
  ------------------------------------------------------------------------------------------------------
  If PKG_Val.Metodo(GLO_Titulo) = 'N' Then
    pr_nuevos_flujos;
    Return(0);
  End If;
  ------------------------------------------------------------------------------------------------------
  -- De aqui en adelante todo se comprorta como estaba sin el nuevo valorador
  ------------------------------------------------------------------------------------------------------
  -- Carga el codigo del pais de la empresa
  select emp_pai_codigo
    into fl_pais
    from pfempresas_v2
   where emp_codigo = glo_empresa
  ;
  -- Guarda el tipo de anualidad para construir el flujo
  select nvl(ref_valor,'') into anualidad_flujo
  from   pfreferencias_v2
  where  ref_codigo = 'DAF'
  ;
  -- Guardar el rango del flujo a definir.
  insert into pfrangos_flujos_v2
    (rfl_inv_consec,rfl_flu_codigo,rfl_f_desde,rfl_f_hasta)
  values
    (pff_inversion,glo_flujo,pff_f_desde,pff_f_hasta)
  ;
  f_ant := null;
  for co in c_operaciones loop
    if glo_repo = 'S' then co.anualidad := co.anualidad_por; end if;
    f_inicial := to_date(fbd_evalue(glo_titulo      -- Consec Titulo
                                   ,pff_inversion   -- Consec Inversion
                                   ,co.consecutivo  -- Consec operacion flujo
                                   ,glo_f_emision   -- Fecha de emision
                                   ,glo_f_emision   -- Solo requerido en valor
                                --   ,glo_ano         -- Anualidad inversion
                                -- Flujos siempre se crean en 360.
                                   ,co.anualidad    -- Anualidad inversion
                                   ,'I'             -- Expresion Inicial
                                   ,null            -- Nominal actual (NA)
                                   ,null            -- Fecha Base
                                   ,pff_f_desde
                                   ,pff_f_hasta
                                   )
                        ,'YYYYMMDD');
    dbms_output.put_line('FI '||to_char(f_inicial));
    dbms_output.put_line('Periodo '||co.periodo);
    -- El dia que se debe respetar para el pago de rendimientos sera siempre
    -- el del resultado de la evaluacion inicial, excepto cuando la periodi-
    -- cidad del flujo es multiplo de 30, y el dia inicial corresponde a
    -- un ultimo dia de mes (en 360), en cuyo caso gobierna el de la fecha
    -- de emision.
    if mod(co.periodo,30) = 0 then
      if fbd_dia(glo_f_emision) <> fbd_dia(f_inicial) and last_day(f_inicial) <> f_inicial then
        glo_dia := fbd_dia(f_inicial);
      Else If glo_f_emision = Last_Day(glo_f_emision) and f_inicial = Last_Day(f_inicial) and fbd_dia(glo_f_emision) <> fbd_dia(f_inicial) then
             glo_dia := fbd_dia(glo_f_emision);
           Else
             glo_dia := fbd_dia(f_inicial);
           End If;
      end if;
    else
      glo_dia := fbd_dia(f_inicial);
    end if;
    dbms_output.put_line('Dia '||to_char(glo_dia));
    -- RRC 1998/01/14  Fecha anterior mayor entre emision y primer flujo
    if mod(co.periodo,30) = 0 then
       f_ant := fbd_sume_meses(f_inicial,-round(co.periodo/30,0),glo_dia);
       if f_ant < glo_f_emision then f_ant := glo_f_emision; end if;
    else
       if f_ant is null then
         f_ant := glo_f_emision; -- Por si hay flujos dependientes
       else       -- Para flujos irregulares anualidad 360 Caso 01122702
         f_ant := f_temp;
       end if;
    end if;
    -- f_ant     := glo_f_emision; -- Por si hay flujos dependientes
    f_temp    := f_inicial;
    f_final   := to_date(fbd_evalue(glo_titulo      -- Consec Titulo
                                   ,pff_inversion   -- Consec Inversion
                                   ,co.consecutivo  -- Consec operacion flujo
                                   ,glo_f_emision   -- Fecha de emision
                                   ,glo_f_emision   -- Solo requerido en valor
                                --   ,glo_ano         -- Anualidad inversion
                                -- Flujos siempre se crean en 360.
                                   ,co.anualidad    -- Anualidad inversion
                                   ,'F'             -- Expresion Final
                                   ,null            -- Nominal actual (NA)
                                   ,null            -- Fecha Base
                                   ,pff_f_desde
                                   ,pff_f_hasta
                                   )
                        ,'YYYYMMDD');
    dbms_output.put_line('FF '||to_char(f_final));
    -- if f_final < f_inicial then f_final := f_inicial; end if;
    -- La anterior linea es necesario incluirla para inversiones realizadas
    -- el 31 de un mes con pago de rendimientos periodo anticipado. La razon
    -- es que los flujos se definen como desde FE hasta FV-PE pero FV-PE puede
    -- ser menor que FE.
    -- Caso 00080402
    t_tipo_periodicidad := null;
    for ict in c_tipo_periodicidad(co.consecutivo) loop
      t_tipo_periodicidad := ict.tipo_periodicidad;
      exit;
    end loop;
    t_tipo_periodicidad := nvl(t_tipo_periodicidad,'PE');
    -- Fin Caso 00080402
    -- if t_tipo_periodicidad = 'PU' then
    -- Caso 07092702
    if t_tipo_periodicidad in ('PU','PP') then
      f_temp := last_day(glo_f_emision);  -- 07082929 fn_ultimo_habil(glo_f_emision);
    end if;

    while f_temp <= f_final loop -- Generar las fechas del flujo
      max_fl := max_fl + 1;
      -- if t_tipo_periodicidad <> 'PU' then
      -- Caso 07092702
      if t_tipo_periodicidad not in ('PU','PP') then
        fl_fecha (max_fl) := f_temp;
      else
          if t_tipo_periodicidad = 'PU' then fl_fecha(max_fl)  := last_day(f_temp); --  07082929 fn_ultimo_habil(f_temp);
    else fl_fecha(max_fl)  := fn_primer_habil(last_day(f_temp))-1; -- 08010852 se adiciona -1
    end if;
        if fl_fecha(max_fl) > f_final then fl_fecha(max_fl) := f_final; end if;
      end if;
      fl_oper  (max_fl)    := co.operacion;
      fl_exv   (max_fl)    := co.consecutivo;
      fl_recalc(max_fl)    := f_ant;
      fl_modo  (max_fl)    := fn_modo(co.consecutivo);
      fl_corregido(max_fl) := fn_corregido(co.consecutivo);
      f_ant  := f_temp; -- Para mantener el anterior periodo
      if f_temp = f_final then exit; end if;
      if mod(co.periodo,30) = 0 then
           f_temp := fbd_sume_meses(f_temp,round(co.periodo/30,0),glo_dia);
      else
        if glo_repo = 'S' then
          f_temp := fbd_sume_dias(f_temp,co.periodo,co.anualidad);
        else
          if anualidad_flujo <> '365D' then
            f_temp := fbd_sume_dias(f_temp,co.periodo,glo_ano);
          else
            f_temp := fbd_sume_dias(f_temp,co.periodo,'365');
          end if;
        end if;
      end if;
    end loop;
  end loop;
  pr_ordene_flujo;       -- Ordenar el flujo para respetar jerarquia
  pr_ajuste_recalculos;  -- Ajusta las fechas de recalculo con base en modo
  pr_ajuste_habiles;     -- Ajusta fechas al anterior o sgte dia habil segun indique la especie.  -- ORC 2006/05/25
  pr_verifique_recalculos; -- Garantiza que toda fecha recalculo corresponda a una de operacion.
  pr_genere_operaciones; -- Genera las operaciones del flujo
  pbd_reevalue(glo_titulo
              ,glo_f_emision
              ,0
              ,greatest(nvl(glo_primer_flujo,glo_f_emision),glo_ft_variable)
              ,glo_ano
              ,pff_f_desde
              ,pff_f_hasta
              ); -- Reevalua
  -- MCE Nov 19/98. Si se genera un flujo posterior al vcto ==> Error
  if fbd_modalidad(pff_inversion) = 'V' and glo_vcto < f_final then
     glo_error := -1;
  end if;
  return(glo_error);
end fbd_flujo;





/
-------------------------------------------------------------------------------
Show err
