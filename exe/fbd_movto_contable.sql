-------------------------------------------------------------------------------
Prompt Compilando fbd_movto_contable
-------------------------------------------------------------------------------
Create or Replace
FUNCTION FBD_MOVTO_CONTABLE ( pfm_empresa     varchar2 -- código de la empresa
                                              ,pfm_portafolio  varchar2 -- Código del portafolio
                                              ,pfm_fecha       date     -- Fecha de proceso
                                              ,pfm_elimina     varchar2 -- Elimina las operaciones
                                             ) return number
is
  --------------------------------------------------------------------------------------------------
  -- Objetivo: Generar el movimiento contable para todas las operaciones en estado OPC a la fecha
  --           pfm_fecha para el portafolio pfm_portafolio (empresa, portafolio).
  --           Nota: Si pfm_elimina = 'S', elimina las operaciones, de lo contrario las deja en
  --                 estado 'OCA'
  -- Parámetros: pfm_empresa:     Empresa
  --             pfm_portafolio: Devuelve OK si el proceso fue exitoso, si no devuelve mensaje
  --             pfm_fecha:
  --             pfm_elimina:
  --------------------------------------------------------------------------------------------------
  -- Modificaciones:
  -- CASO     CONSULTOR FECHA      DESCRIPCION CAMBIO
  -- 11072508 SUMH
  --------------------------------------------------------------------------------------------------
  -------------------------------------------------------------------------
  -- Declaracion de tipos de vectores locales a PORFIN_P.fbd_movto_contable
  -------------------------------------------------------------------------
   TYPE vec_operacion     IS TABLE OF PFOPERACIONES_V2.ope_consec%TYPE         INDEX BY BINARY_INTEGER;
   TYPE vec_tipo_concepto IS TABLE OF VARCHAR2(2)                              INDEX BY BINARY_INTEGER;
   TYPE vec_concepto      IS TABLE OF PFCUENTAS_AFECTADAS_V2.caf_concepto%TYPE INDEX BY BINARY_INTEGER;  -- ORC 20071027
   TYPE vec_tipo_valor    IS TABLE OF VARCHAR2(2)                              INDEX BY BINARY_INTEGER;
   TYPE vec_valor         IS TABLE OF NUMBER                                   INDEX BY BINARY_INTEGER;
   -------------------------------------------------------------------------
   -- Declaracion de vectores locales a fbd_movto_contable
   -------------------------------------------------------------------------
   -- Vectores para guardar los valores de la operaciones
   -------------------------------------------------------------------------
   va_operacion     vec_operacion     ;  -- Consecutivo operacion
   va_tipo_concepto vec_tipo_concepto ;  -- Tipo de concepto contable (Financiero, Trib.)
   va_concepto      vec_concepto      ;  -- Concepto contable
   va_tipo_valor    vec_tipo_valor    ;  -- Tipo de valor del concepto contable ($, U, etc)
   va_valor         vec_valor         ;  -- Valor concepto
   max_va           BINARY_INTEGER := 0; -- Dimensionamiento de va
   -------------------------------------------------------------------------
   -- Definicion de cursores
   -------------------------------------------------------------------------
     -- Seleccion de la informacion de cada una de las operaciones
     -------------------------------------------------------------------------
     CURSOR c_mvto_operaciones IS
       SELECT
              ope_consec                            operacion
             ,ope_fecha                             fecha_operacion
             ,ope_f_cobro                           fecha_cobro
             ,ope_top_codigo                        tipo_operacion
             ,ope_int_codigo                        intermediario
             ,ope_tit_consec                        titulo
             ,ope_valor                             valor
             ,ope_precio                            precio
             ,NVL(ope_cnv_precio,1)                 cnv_precio
             ,ope_suc_origen                        o_suc_origen
             ,ope_suc_destino                       o_suc_destino
             ,top_calculo_contable                  calculo -- (D o F)
             ,DECODE(por_activo_pasivo
                     ,'E'
                     ,top_desc_pasivo
                     ,top_descripcion)              descripcion
             ,DECODE(NVL(cin_contraparte_repo,'E')
                    ,'E',NVL(esp_administrador,emi_per_ident)
                    ,'T',inv_per_ident_compra
                    )                               nit
             ,ope_per_ident                         nit_tercero
             ,ope_cnv_codigo                        moneda_operacion
             ,inv_cnv_codigo                        unidad
             ,inv_cnv_cnv_codigo                    foranea
             ,inv_emp_codigo                        empresa
             ,inv_por_codigo                        portafolio
             ,inv_cin_codigo                        clase_inversion
             ,inv_tin_codigo                        tipo_inversion
             ,inv_emi_codigo                        emisor
             ,inv_mercado                           mercado
             ,inv_efe_compra                        tasa_compra
             ,TO_CHAR(NULL)                         tasa_nominal
             ,DECODE(por_activo_pasivo
                    ,'D',inv_proposito
                        ,NVL(tit_fuente,inv_fuente)
                     )                              fuente
             ,esp_nombre                            especie
             ,TO_CHAR(NULL)                         clasificacion
             ,por_clase                             clase
             -- Por ahora solo divisas Operacion Compra/Venta
             ,DECODE(ope_top_codigo
                    ,'1',NVL(inv_tipo_movto,NVL(por_tipo_movto,'___'))
                    ,'2',NVL(inv_tipo_movto,NVL(por_tipo_movto,'___'))
                        ,NVL(por_tipo_movto,'___')
                    )
                                                    tipo_movto
             ,TO_CHAR(NULL)                         modalidad
             ,NVL(por_activo_pasivo,'A')            activo_pasivo
             ,TO_CHAR(NULL)                         beneficiario
             ,TO_CHAR(NULL)                         garantia
             ,Fbd_Plazo(NVL(tit_f_venta,NVL(inv_f_vigencia,inv_f_vcto))
                 ,inv_f_compra
                 ,inv_anualidad
                 )                                  plazo
             ,inv_plazo                             plazo_facial
             ,Fbd_Plazo(inv_f_vcto
                       ,inv_f_compra
                       ,inv_anualidad
                       )                            plazo_vcto_comp
             ,inv_f_emision
             ,inv_f_compra
             ,inv_f_vcto
             ,inv_sue_codigo                        sucursal   -- orc 9910127
             ,inv_pago_bancos                       banco
             ,DECODE(ope_top_codigo
                    ,'cr',Fbd_Referencia_Contable(tit_consec,ope_consec,pfm_fecha,'AN')
                         ,Fbd_Referencia_Contable(tit_consec,ope_consec,pfm_fecha,'AC')
                     )                              ref_contable
             ,Fbd_Ctro_Costo(ope_consec)            ctro_costo
             ,esp_bursatilidad                      bursatilidad
             ,Fbd_Sector_Alterno(NVL(ope_per_ident,inv_per_ident_compra))                                  sector_propio
             ,DECODE(ope_top_codigo
                    ,'CGD',Fbd_Sector_Alterno(NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'NIT','A'),NVL(ope_per_ident,inv_per_ident_compra)))
                    ,'CGD1',fbd_sector_alterno(ope_per_ident)  --ELBP 2008/09/22
                    ,'FGD',fbd_sector_alterno(ope_per_ident)   --JAMR cASO 09061815
                    ,'U'  ,Fbd_Sector_Alterno(NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'NIT','I'),NVL(ope_per_ident,inv_per_ident_compra)))
                    ,'U1' ,Fbd_Sector_Alterno(NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'NIT','I'),NVL(ope_per_ident,inv_per_ident_compra)))
                          ,Fbd_Sector_Alterno(NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'NIT','*'),NVL(ope_per_ident,inv_per_ident_compra)))
                    )                                                                                      sector_garantizado
             ,NVL(ope_rco_codigo,NVL(tit_cln_codigo,'*'))                                                  clase_negocio_propia
             ,NVL(ope_cln_codigo,NVL(tit_cln_codigo,'*'))                                                  clase_negocio  -- ORC 20071027
             ,DECODE(ope_top_codigo
                    ,'CGD',NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO','A'),NVL(ope_rco_codigo,NVL(tit_cln_codigo,'*')))
                    ,'CGD1',fbd_sector_alterno(ope_per_ident)  --ELBP 2008/09/22
                    ,'FGD',fbd_sector_alterno(ope_per_ident)   --JAMR cASO 09061815
                    ,'U'  ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO','I'),NVL(ope_rco_codigo,NVL(tit_cln_codigo,'*')))
                    ,'U1' ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO','I'),NVL(ope_rco_codigo,NVL(tit_cln_codigo,'*')))
                          ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO','*'),NVL(ope_rco_codigo,NVL(tit_cln_codigo,'*')))
                    )                                                                                      clase_negocio_garantizada
             --,nvl(fbd_operacion_garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO','*'),'*')                 clase_negocio_liq
            ,nvl(fbd_operacion_garantizada(tit_consec,ope_fecha,'CLASE_NEGOCIO',decode(ope_top_codigo
                                                                                       ,'U','I'
                                                                                       ,'W','I'
                                                                                           ,'*'
                                                                                       ),decode(ope_top_codigo   -- JCD 20080430
                                                                                               ,'T','T'          -- JCD 20080430
                                                                                                   ,null         -- JCD 20080430
                                                                                               ),ope_top_codigo),'*')     clase_negocio_liq         -- JCD 20080402  -- JCD 20080430
             ,Fbd_Negocio_Ori(ope_deg_consec)                                                              clase_negocio_ori         -- JCD 20080402
             ,Fbd_Origen_Garantia(tit_consec,ope_fecha)                                                    origen_titulo_propio
             ,DECODE(ope_top_codigo
                    ,'CGD',NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'ORIGEN','A'),Fbd_Origen_Garantia(tit_consec,ope_fecha))
                    ,'CGD1',fbd_sector_alterno(ope_per_ident)  --ELBP 2008/09/22
                    ,'FGD',fbd_sector_alterno(ope_per_ident)   --JAMR cASO 09061815
                    ,'U'  ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'ORIGEN','I'),Fbd_Origen_Garantia(tit_consec,ope_fecha))
                    ,'U1' ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'ORIGEN','I'),Fbd_Origen_Garantia(tit_consec,ope_fecha))
                          ,NVL(Fbd_Operacion_Garantizada(tit_consec,ope_fecha,'ORIGEN','*'),Fbd_Origen_Garantia(tit_consec,ope_fecha))
                    )                                                                                      origen_titulo_garantizado
             ,ope_f_compromiso                      f_compromiso
             ,ope_candidata_forward                 c_forward
             ,NVL(cin_swap,'N')                     c_swap
             ,inv_consec                            inversion
             ,por_activo_pasivo
             ,inv_tipo_operacion
             ,inv_f_suscripcion
             /*,nvl(fbd_operacion_garantizada(tit_consec,ope_fecha,'NIT','*'),decode(fbd_cliente
                                                                                  ,'o17',null
                                                                                        ,nvl(ope_cln_codigo,nvl(tit_cln_codigo,'*'))
                                                                                  )
                 )                                                                                         nit_tercero_garantizado  -- ORC 20080826*/
             ,nvl(fbd_operacion_garantizada(tit_consec,ope_fecha,'NIT','*'),decode(fbd_cliente
                                                                                  ,'o17',null
                                                                                        ,nvl(ope_cln_codigo,nvl(tit_cln_codigo,null))
                                                                                  )
                 )                                                                                         nit_tercero_garantizado  -- ORC 20080826
             ,tit_tit_obligacion            obligacion
         FROM PFEMISORES_V2
             ,PFCLASE_INVERSIONES_V2
             ,PFESPECIES_V2
             ,PFTIPO_OPERACIONES_V2
             ,PFINVERSIONES_V2
             ,PFTITULOS_V2
             ,PFOPERACIONES_V2
             ,PFPORTAFOLIOS_V2
        WHERE emi_codigo        = inv_emi_codigo
          AND cin_codigo        = inv_cin_codigo
          AND esp_cin_codigo    = inv_cin_codigo
          AND esp_tin_codigo    = inv_tin_codigo
          AND esp_emi_codigo    = inv_emi_codigo
          AND inv_consec        = tit_inv_consec
          AND tit_estado_adm    = 'UOK'  -- Para que no tome los excluidos.
          AND tit_consec        = ope_tit_consec
          AND top_codigo        = ope_top_codigo
          AND ope_flujo_real||''= 'S'
          --AND ope_estado        = 'OPC'
          AND ope_estado||''        = 'OPC' -- 25-03-2011
          AND ope_emp_codigo    = por_emp_codigo
          AND ope_por_codigo    = por_codigo
          AND por_emp_codigo    = pfm_empresa
          AND por_codigo        = pfm_portafolio
     ORDER BY inv_consec
             ,tit_consec
     ;
     -------------------------------------------------------------------------
     -- Seleccion de las cuentas que se afectan
     -------------------------------------------------------------------------
     CURSOR c_cuentas(p_tipo_movto       VARCHAR2
                     ,p_clasificacion    VARCHAR2
                     ,p_tipo_operacion   VARCHAR2
                     ,p_fuente           VARCHAR2
                     ,p_modalidad        VARCHAR2
                     ,p_mercado          VARCHAR2
                     ,p_activo_pasivo    VARCHAR2
                     ,p_garantia         VARCHAR2
                     ,p_ref_contable     VARCHAR2
                     ,p_clase_negocio     VARCHAR2 -- ORC 20071027
                     ,p_clase_negocio_liq VARCHAR2 -- ORC 20071205
                     ,p_clase_negocio_ori VARCHAR2 -- JCD 20080402
                     ) IS
       SELECT
              caf_cuenta                 cuenta
             ,cta_ref_codigo             agrupamiento
             ,SUBSTR(cta_auxiliar ,3,1)  auxiliar
             ,SUBSTR(cta_documento,3,1)  documento
             ,SUBSTR(cta_referencia,3,1) referencia
             ,caf_requiere_nit           tipo_nit
             ,caf_naturaleza             naturaleza
             ,caf_porcentaje             porcentaje
             ,caf_concepto               concepto
             ,caf_tipo_valor             tipo_valor  -- ORC 1999/03/29
             ,caf_signo                  signo
             ,caf_monto_base             monto_base
             ,caf_operacion              operacion
             ,caf_comprobante            comprobante
             ,caf_procedencia            procedencia
             ,caf_tipo_concepto          tipo_concepto
             ,caf_suc_origen             c_suc_origen
             ,caf_suc_destino            c_suc_destino
             ,caf_auxiliar1              auxiliar1
         FROM PFCUENTAS_V2
             ,PFCUENTAS_AFECTADAS_V2
        WHERE cta_cuenta         = caf_cuenta
          AND caf_top_codigo     = p_tipo_operacion
          AND caf_tipo_movto     = p_tipo_movto
          AND caf_clasificacion  = p_clasificacion
          AND caf_fuente         = p_fuente
          AND caf_cto_modalidad  = p_modalidad
          AND caf_cto_mercado    = p_mercado
          AND caf_activo_pasivo  = p_activo_pasivo
          AND caf_garantia       = p_garantia
          AND caf_rco_codigo     = p_ref_contable
          AND caf_cln_codigo     = p_clase_negocio      -- ORC 20071027
          AND caf_cln_codigo_liq = p_clase_negocio_liq  -- ORC 20071205
          AND caf_cln_codigo_ori = p_clase_negocio_ori  -- JCD 20080402
       ORDER BY caf_secuencia,caf_cuenta
     ;

     -------------------------------------------------------------------------
     -- Seleccion de las cuentas que se afectan
     -------------------------------------------------------------------------
     CURSOR c_det_garantia(pcg_titulo NUMBER) IS
       SELECT
              deg_tit_consec   titulo
             ,deg_valor        valor
             ,deg_f_desde      f_desde
             ,deg_f_hasta      f_hasta
             ,tit_cin_codigo   clase
             ,tit_tin_codigo   tipo
             ,tit_emi_codigo   emisor
         FROM PFTITULOS_V2
             ,PFDETALLE_GARANTIAS_V2
             ,PFGARANTIAS_ESTABLECIDAS_V2
        WHERE tit_consec     = deg_tit_consec
          AND deg_soporte    = 'T'
          AND deg_ges_consec = ges_consec
          AND ges_tit_consec = pcg_titulo
     ;

   CURSOR c_cuentas_especie_gtia(pcc_tipo_movto                VARCHAR2
                                ,pcc_top_codigo                VARCHAR2
                                ,pcc_clase                     VARCHAR2
                                ,pcc_tipo                      VARCHAR2
                                ,pcc_emisor                    VARCHAR2
                                ,pcc_clase_negocio_propia      VARCHAR2
                                ,pcc_clase_negocio_garantizada VARCHAR2
                                ) IS
     SELECT ceg_cuenta
           ,ceg_cuenta_a
           ,ceg_cuenta_b
           ,DECODE(ceg_top_codigo,'*',0,1) * 10 + DECODE(ceg_cln_codigo,'*',0,1) * 1
       FROM PFCUENTAS_ESPECIE_GARANTIA_V2
      WHERE ceg_ref_codigo = pcc_tipo_movto
        AND ceg_cin_codigo = pcc_clase
        AND ceg_tin_codigo = pcc_tipo
        AND ceg_emi_codigo = pcc_emisor
        AND ((ceg_top_codigo = pcc_top_codigo AND ceg_cln_codigo = DECODE(ceg_clase_negocio_base,'P',pcc_clase_negocio_propia,pcc_clase_negocio_garantizada)) OR
             (ceg_top_codigo = '*'            AND ceg_cln_codigo = DECODE(ceg_clase_negocio_base,'P',pcc_clase_negocio_propia,pcc_clase_negocio_garantizada)) OR
             (ceg_top_codigo = pcc_top_codigo AND ceg_cln_codigo = '*'                                                                                  ) OR
             (ceg_top_codigo = '*'            AND ceg_cln_codigo = '*')
            )
      ORDER BY 4 DESC
   ;

   -------------------------------------------------------------------------
   -- Variables locales al procedimiento
   -------------------------------------------------------------------------
   w_nit                     PFPERSONAS_V2.per_ident%TYPE;               -- Nit
   w_valor                   PFMOVTOS_CONTABLES_V2.mco_valor%TYPE ;      -- Valor en $ del movto contable
   w_valor_detalle           PFMOVTOS_CONTABLES_V2.mco_valor%TYPE ;      -- Valor en $ del movto contable
   t_total_garantias         NUMBER;
   w_cuenta                  VARCHAR2(16) ;                              -- Numero de cuenta si (#e,#t,#c)
   w_cuenta_base             varchar2(16) ;                              -- Numero de cuenta con (#e,#t,#c) -- 08042916
   w_cuenta_detalle          VARCHAR2(16) ;                              -- Numero de cuenta sin (#N)
   w_descripcion             VARCHAR2(200);                              -- Descripcion de la cuenta contable
   w_naturaleza              VARCHAR2(1)  ;                              -- Naturaleza (D-ebito o C-redito)
   w_modalidad               VARCHAR2(1)  ;                              -- Modalidad (A-nticipado o V-encido)
   w_mercado                 VARCHAR2(1)  ;                              -- Mercado (P-rimario o S-ecundario)
   w_empresa                 PFEMPRESAS_V2.emp_codigo%TYPE;              -- Codigo de la empresa
   w_portafolio              PFPORTAFOLIOS_V2.por_codigo%TYPE;           -- Codigo del portafolio
   w_clasificacion           PFREFERENCIAS_V2.ref_codigo%TYPE;           -- Negociable o Permanente
   w_clase                   PFREFERENCIAS_V2.ref_codigo%TYPE;           -- Renta fija o renta variable
   w_suc_origen              PFMOVTOS_CONTABLES_V2.mco_suc_origen%TYPE ; -- Oficina origen de la operacion
   w_suc_destino             PFMOVTOS_CONTABLES_V2.mco_suc_destino%TYPE; -- Oficina destino de la transaccion
   consecutivo               PFMOVTOS_CONTABLES_V2.mco_consec%TYPE;      -- Consecutivo del movto contable
   t_error                   NUMBER       ;                              -- Manejo de errores en llamados a funciones
   t_fuente                  PFREFERENCIAS_V2.ref_codigo%TYPE  ;
   t_modalidad               VARCHAR2(1)  ;
   t_clase_negocio           PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;        -- ORC 20071027
   t_clase_negocio_liq       PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;        -- ORC 20071205
   t_clase_negocio_ori       PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;        -- JCD 20080402
   t_mercado                 VARCHAR2(1)  ;
   t_garantia                VARCHAR2(1)  ;
   t_cta_bur                 PFCUENTAS_BURSATILIDAD_V2.cbu_cuenta%TYPE;
   t_cta_bur_a               PFCUENTAS_BURSATILIDAD_V2.cbu_cuenta_a%TYPE;
   t_cta_riesgo              PFNIVELES_CALIFICACION_V2.nic_cuenta%TYPE;
   t_cta_riesgo_a            PFNIVELES_CALIFICACION_V2.nic_cuenta_a%TYPE;
   t_cta_riesgo_ayer         PFNIVELES_CALIFICACION_V2.nic_cuenta%TYPE;  -- 11072508
   t_cta_riesgo_a_ayer       PFNIVELES_CALIFICACION_V2.nic_cuenta_a%TYPE;-- 11072508
   --t_por_anterior_cierre     PFPORTAFOLIOS_V2.POR_ANTERIOR_CIERRE%TYPE;  --11072508
   t_fecha_cierre            PFPORTAFOLIOS_V2.POR_FECHA_CIERRE%TYPE;     --11072508
   t_cta_clase_inversion     PFCUENTAS_CLASE_V2.ctc_cuenta%TYPE;
   t_cta_clase_inversion_a   PFCUENTAS_CLASE_V2.ctc_cuenta_a%TYPE;
   t_cta_tipo_inversion      PFCUENTAS_TIPO_V2.ctt_cuenta%TYPE;
   t_cta_tipo_inversion_a    PFCUENTAS_TIPO_V2.ctt_cuenta_a%TYPE;
   t_cta_emisor              PFCUENTAS_EMISOR_V2.cte_cuenta%TYPE;
   t_cta_emisor_a            PFCUENTAS_EMISOR_V2.cte_cuenta_a%TYPE;
   t_cta_plazo               PFCUENTAS_PLAZO_V2.cpl_cuenta%TYPE;
   t_cta_plazo_a             PFCUENTAS_PLAZO_V2.cpl_cuenta_a%TYPE;
   t_cod_plazo               VARCHAR2(2);                                -- Codigo del plazo asociado a la captacion
   t_cta_especie             PFCUENTAS_ESPECIE_V2.cts_cuenta%TYPE;
   t_cta_especie_a           PFCUENTAS_ESPECIE_V2.cts_cuenta_a%TYPE;
   t_cta_sucursal            PFSUCURSALES_EMPRESA_V2.sue_cuenta%TYPE;
   t_cta_cod_banco           PFSUCURSALES_EMPRESA_V2.sue_cuenta%TYPE;
   t_cta_plazo_aut           PFAUTORETENCION_PLAZO_V2.apl_cuenta%TYPE;
   t_cta_plazo_aut_a         PFAUTORETENCION_PLAZO_V2.apl_cuenta_alterna%TYPE;
   t_base                    PFMOVTOS_CONTABLES_V2.mco_valor%TYPE;
   t_debitos                 NUMBER;
   t_creditos                NUMBER;
   t_plazo                   NUMBER;
   t_nit                     PFPERSONAS_V2.per_ident%TYPE;
   glo_titulo                PFTITULOS_V2.tit_consec%TYPE := 0;
   glo_round                 PFEMPRESAS_V2.emp_cnv_codigo%TYPE := 2;
   t_valor_base              PFMOVTOS_CONTABLES_V2.mco_valor%TYPE;
   t_cta_sector              PFCUENTAS_SECTOR_V2.csc_cuenta%TYPE;
   t_cta_sector_a            PFCUENTAS_SECTOR_V2.csc_cuenta_a%TYPE;
   t_cta_origen_garantia     PFCUENTAS_ORIGEN_GARANTIA_V2.cog_cuenta%TYPE;
   t_cta_origen_garantia_a   PFCUENTAS_ORIGEN_GARANTIA_V2.cog_cuenta_a%TYPE;
   t_cta_clase_inversion_b   PFCUENTAS_CLASE_V2.ctc_cuenta_b%TYPE;
   t_cta_tipo_inversion_b    PFCUENTAS_TIPO_V2.ctt_cuenta_b%TYPE;
   t_cta_emisor_b            PFCUENTAS_EMISOR_V2.cte_cuenta_b%TYPE;
   t_cta_especie_b           PFCUENTAS_ESPECIE_V2.cts_cuenta_b%TYPE;
   t_cta_sector_b            PFCUENTAS_SECTOR_V2.csc_cuenta_b%TYPE;
   t_cta_origen_garantia_b   PFCUENTAS_ORIGEN_GARANTIA_V2.cog_cuenta_b%TYPE;
   t_cta_plazo_b             PFCUENTAS_PLAZO_V2.cpl_cuenta_b%TYPE;
   t_cta_bur_b               PFCUENTAS_BURSATILIDAD_V2.cbu_cuenta_b%TYPE;
   t_activo_pasivo           PFPORTAFOLIOS_V2.por_activo_pasivo%TYPE;
   t_pla_clase_inv           PFPLAZOS_EMISION_V2.pem_cin_codigo%TYPE;
   t_pla_tipo_inv            PFPLAZOS_EMISION_V2.pem_tin_codigo%TYPE;
   glo_round_ant             PFEMPRESAS_V2.emp_cnv_codigo%TYPE := NULL;  -- Caso 04052718
   t_moneda_decimales        PFCONVERSIONES_V2.cnv_codigo%TYPE;          -- Caso 04052718
   glo_tasa_nominal          VARCHAR2(100);
   glo_clasificacion         VARCHAR2(100);
   glo_modalidad             VARCHAR2(100);
   glo_beneficiario          PFBENEFICIARIOS_TITULO_V2.bxt_per_ident%TYPE;
   glo_garantia              VARCHAR2(10);
   glo_inv                   NUMBER := -1;
   glo_tit                   NUMBER := -1;
   t_anualidad_nro_l         PFREFERENCIAS_V2.ref_valor%TYPE;
   t_tm                      NUMBER;
   t_fecha                   DATE;
   t_concepto                VARCHAR2(10);                               -- ORC 20071027
   t_tmp_valor               NUMBER := 0;                                -- ORC 20071027
   t_cuantos                 NUMBER := 1;                                -- ORC 20071027
   t_cuantos_cont                        number := 0;                    -- ORC 20071027
   t_pos_concepto            number := 1;                                -- 08102911
   t_constante               number := 3;                                -- 08102911
   t_cta_moneda              pfcuentas_moneda_v2.ctm_cuenta%type;        --08102911. 20090520
   t_cta_moneda_a            pfcuentas_moneda_v2.ctm_cuenta_a%type;      --08102911. 20090520
   t_cta_moneda_b            pfcuentas_moneda_v2.ctm_cuenta_b%type;      --08102911. 20090520
   t_der_obl                 varchar2(1);
   --
   t_error_controlado        exception;      -- 12041322 Patch PP20831
   t_mensaje_error           varchar2(2000); -- 12041322 Patch PP20831
   -------------------------------------------------------------------------
   -- Definicion de procedimientos y funciones locales a FBD_MOVTO_CONTABLE
   -------------------------------------------------------------------------
   PROCEDURE pr_ct_oper( ppc_tipo_movto               VARCHAR2
                        ,ppc_operacion                VARCHAR2
                        ,ppc_clasificacion            VARCHAR2
                        ,ppc_fuente            IN OUT VARCHAR2
                        ,ppc_mercado           IN OUT VARCHAR2
                        ,ppc_modalidad         IN OUT VARCHAR2
                        ,ppc_activo_pasivo            VARCHAR2
                        ,ppc_garantia          IN OUT VARCHAR2
                        ,ppc_ref_contable             VARCHAR2
                        ,ppc_clase_negocio     IN OUT VARCHAR2   -- ORC 20071027
                        ,ppc_clase_negocio_liq IN OUT VARCHAR2   -- ORC 20071205
                        ,ppc_clase_negocio_ori IN OUT VARCHAR2   -- JCD 20080402
                       )
  IS
   -------------------------------------------------------------------------
   -- Dados la fuente, mercado, y modalidad, retorna el c_concepto por tipo
   -- de operacion
   -------------------------------------------------------------------------
      -------------------------------------------------------------------------
      -- Definicion cursores
      -------------------------------------------------------------------------
        CURSOR c_concepto (tc_tipo_movto    VARCHAR2
                        ,tc_operacion     VARCHAR2
                        ,tc_clasificacion VARCHAR2
                        ,tc_fuente        VARCHAR2
                        ,tc_modalidad     VARCHAR2
                        ,tc_mercado       VARCHAR2
                        ,tc_activo_pasivo VARCHAR2
                        ,tc_garantia      VARCHAR2
                        ,tc_ref_contable  VARCHAR2
                         ,tc_clase_negocio     VARCHAR2   -- ORC 20071027
                         ,tc_clase_negocio_liq VARCHAR2   -- ORC 20071205
                         ,tc_clase_negocio_ori VARCHAR2   -- JCD 20080402
                        ) IS
            SELECT
                cto_fuente
               ,cto_mercado
               ,cto_modalidad
               ,cto_garantia
               ,cto_cln_codigo                                -- ORC 20071027
               ,cto_cln_codigo_liq                            -- ORC 20071205
               ,cto_cln_codigo_ori                            -- JCD 20080402
               ,DECODE(cto_fuente        ,'*',0,1) * 1000000 +     -- ORC 20071027  -- ORC 20071205  -- JCD 20080402
                DECODE(cto_mercado       ,'*',0,1) * 100000  +     -- ORC 20071027  -- ORC 20071205  -- JCD 20080402
                DECODE(cto_modalidad     ,'*',0,1) * 10000   +     -- ORC 20071027  -- ORC 20071205  -- JCD 20080402
                DECODE(cto_garantia      ,'*',0,1) * 1000    +     -- ORC 20071027  -- ORC 20071205  -- JCD 20080402
                DECODE(cto_cln_codigo    ,'*',0,1) * 100     +     -- ORC 20071027  -- ORC 20071205  -- JCD 20080402
                DECODE(cto_cln_codigo_liq,'*',0,1) * 10      +     -- ORC 20071205  -- JCD 20080402
                DECODE(cto_cln_codigo_ori,'*',0,1) * 1             -- JCD 20080402
              FROM PFCONC_TIPO_OPER_V2
             WHERE (        -- ORC 20071027: Cambiarlo todo dado que ahora clase de negocio maneja asterisco, por tanto de 16 se pasa a 32 OR.
                            -- JCD 20080402: Cambiarlo todo dado que ahora existe clase de negocio origen, por tanto de 64 se pasa a 128.
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = tc_fuente AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = tc_mercado AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = tc_modalidad AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = tc_garantia AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = tc_clase_negocio AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = tc_clase_negocio_liq AND cto_cln_codigo_ori = '*'                 ) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = tc_clase_negocio_ori) OR
                    (cto_fuente     = '*'       AND cto_mercado = '*'        AND cto_modalidad = '*'          AND cto_garantia = '*'         AND cto_cln_codigo = '*'              AND cto_cln_codigo_liq = '*'                  AND cto_cln_codigo_ori = '*'                 )
                   )
               AND cto_tipo_movto    = tc_tipo_movto
               AND cto_top_codigo    = tc_operacion
               AND cto_clasificacion = tc_clasificacion
               AND cto_activo_pasivo = tc_activo_pasivo
               AND cto_rco_codigo    = tc_ref_contable
             ORDER BY 8 DESC       -- ORC 20071027    -- ORC 20071205  -- JCD 20080402
      ;
      -------------------------------------------------------------------------
      -- Definicion  de variables
      -------------------------------------------------------------------------
         t_retorno           c_concepto%ROWTYPE;
         t_fuente    PFREFERENCIAS_V2.ref_codigo%TYPE;
         t_modalidad VARCHAR2(3);
         t_mercado   VARCHAR2(3);
         t_garantia  VARCHAR2(1);
         t_clase_negocio     PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;       -- ORC 20071027
         t_clase_negocio_liq PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;       -- ORC 20071205
         t_clase_negocio_ori PFCLASES_NEGOCIO_V2.cln_codigo%TYPE;       -- JCD 20080402
   -------------------------------------------------------------------------
   BEGIN  -- de pr_ct_oper
     t_fuente            := ppc_fuente;
     t_modalidad         := ppc_modalidad;
     t_mercado           := ppc_mercado;
     t_garantia          := ppc_garantia;
     t_clase_negocio     := ppc_clase_negocio;        -- ORC 20071027
     t_clase_negocio_liq := ppc_clase_negocio_liq;    -- ORC 20071205
     t_clase_negocio_ori := ppc_clase_negocio_ori;    -- JCD 20080402
     --PBD_LOG('fbd_movto_contable',ppc_tipo_movto||','||ppc_operacion||','||ppc_clasificacion||','||t_fuente||','||t_modalidad||','||t_mercado||','||ppc_activo_pasivo
     --                                   ||','||t_garantia||','||ppc_ref_contable||','||t_clase_negocio||','||t_clase_negocio_liq||','||t_clase_negocio_ori);
     OPEN c_concepto(ppc_tipo_movto,ppc_operacion,ppc_clasificacion,t_fuente,t_modalidad,t_mercado,ppc_activo_pasivo
                                        ,t_garantia,ppc_ref_contable,t_clase_negocio,t_clase_negocio_liq,t_clase_negocio_ori);  -- ORC 20071027 -- ORC 20071205 -- JCD 20080402
     FETCH c_concepto INTO t_retorno;
     IF c_concepto%NOTFOUND THEN ppc_fuente            := NULL;
                                 ppc_modalidad         := NULL;
                                 ppc_mercado           := NULL;
                                 t_garantia     := NULL;
                                 ppc_clase_negocio     := NULL;                            -- ORC 20071027
                                 ppc_clase_negocio_liq := NULL;                            -- ORC 20071205
                                 ppc_clase_negocio_ori := NULL;                            -- JCD 20080402
     ELSE                        ppc_fuente            := t_retorno.cto_fuente;
                                 ppc_modalidad         := t_retorno.cto_modalidad;
                                 ppc_mercado           := t_retorno.cto_mercado;
                                 ppc_garantia          := t_retorno.cto_garantia;
                                 ppc_clase_negocio     := t_retorno.cto_cln_codigo;        -- ORC 20071027
                                 ppc_clase_negocio_liq := t_retorno.cto_cln_codigo_liq;    -- ORC 20071205  -- ORC 20071210
                                 ppc_clase_negocio_ori := t_retorno.cto_cln_codigo_ori;    -- JCD 20080402
     END IF;
     CLOSE c_concepto;
   END pr_ct_oper;

   -------------------------------------------------------------------------
   PROCEDURE pr_mensaje (pph_proceso VARCHAR2) IS
   BEGIN
     --dbms_output.put_line(pph_proceso||' '||to_char(sysdate,'hh:mi:ss'));
     dbms_output.put_line(pph_proceso||' ');
     --pbd_log('fbd_movto_contable',pph_proceso);
   END;
   -------------------------------------------------------------------------
   PROCEDURE pn_plazo(ppp_clase    IN OUT VARCHAR2  -- Clase de inversion
                     ,ppp_tipo     IN OUT VARCHAR2  -- Tipo de inversion
                     ,ppp_dias            NUMBER    -- plazo en d¿as
                     ,ppp_codigo      OUT VARCHAR2  -- C¿digo del plazo
                     ,ppp_aplica          VARCHAR2  -- Aplica E emision ¿ F FW de RF
                     ) IS
   -------------------------------------------------------------------------
   -- Retorna el codigo del plazo para buscar la cuenta contable.
   -- En la clase y el tipo puede llegar  '*', que quiere decir que no aplica,
   -- por tanto se debe dar mayor peso cuando estos datos vienen puntuales.
   -------------------------------------------------------------------------
   -- Declaracion de cursores
       CURSOR c_plazo IS
        SELECT pem_codigo
              ,pem_cin_codigo
              ,pem_tin_codigo
              ,DECODE(pem_cin_codigo,'*',0,1) * 10 + DECODE(pem_tin_codigo,'*',0,1) * 1
         FROM PFPLAZOS_EMISION_V2
        WHERE pem_aplica     = ppp_aplica
          AND ((pem_cin_codigo = ppp_clase AND pem_tin_codigo = ppp_tipo) OR
               (pem_cin_codigo = '*'       AND pem_tin_codigo = ppp_tipo) OR
               (pem_cin_codigo = ppp_clase AND pem_tin_codigo = '*')    OR
               (pem_cin_codigo = '*'       AND pem_tin_codigo = '*')
              )
          AND pem_dias      >= ppp_dias
        ORDER BY 4 DESC, pem_dias ASC;
   BEGIN   -- de pn_plazos
       ppp_codigo := '';
       FOR cp IN c_plazo LOOP
           ppp_codigo := cp.pem_codigo;
           ppp_clase  := cp.pem_cin_codigo;
           ppp_tipo   := cp.pem_tin_codigo;
           EXIT;
       END LOOP;
   END pn_plazo;
   -------------------------------------------------------------------------
   PROCEDURE pr_subcuentas(p_tipo_movto                    VARCHAR2
                          ,p_clase                         VARCHAR2
                          ,p_tipo                          VARCHAR2
                          ,p_emisor                        VARCHAR2
                          ,p_plazo                         VARCHAR2
                          ,p_top_codigo                    VARCHAR2
                          ,p_sucursal                      VARCHAR2
                          ,p_banco                         VARCHAR2
                          ,p_moneda                        VARCHAR2
                          ,p_plazo_facial                  NUMBER
                          ,p_bur_codigo                    VARCHAR2
                          ,p_sector_propio                 VARCHAR2
                          ,p_sector_garantizado            VARCHAR2
                          ,p_clase_negocio_propia          VARCHAR2
                          ,p_clase_negocio_garantizada     VARCHAR2
                          ,p_origen_titulo_propio          VARCHAR2
                          ,p_origen_titulo_garantizado     VARCHAR2
                          ,p_cta_clase              IN OUT VARCHAR2
                          ,p_cta_clase_a            IN OUT VARCHAR2
                          ,p_cta_clase_b            IN OUT VARCHAR2
                          ,p_cta_tipo               IN OUT VARCHAR2
                          ,p_cta_tipo_a             IN OUT VARCHAR2
                          ,p_cta_tipo_b             IN OUT VARCHAR2
                          ,p_cta_emisor             IN OUT VARCHAR2
                          ,p_cta_emisor_a           IN OUT VARCHAR2
                          ,p_cta_emisor_b           IN OUT VARCHAR2
                          ,p_cta_plazo              IN OUT VARCHAR2
                          ,p_cta_plazo_a            IN OUT VARCHAR2
                          ,p_cta_plazo_b            IN OUT VARCHAR2
                          ,p_cta_especie            IN OUT VARCHAR2
                          ,p_cta_especie_a          IN OUT VARCHAR2
                          ,p_cta_especie_b          IN OUT VARCHAR2
                          ,p_cta_sucursal           IN OUT VARCHAR2
                          ,p_cta_cod_banco          IN OUT VARCHAR2
                          ,p_cta_plazo_aut          IN OUT VARCHAR2
                          ,p_cta_bur                IN OUT VARCHAR2
                          ,p_cta_bur_a              IN OUT VARCHAR2
                          ,p_cta_bur_b              IN OUT VARCHAR2
                          ,p_cta_sector             IN OUT VARCHAR2
                          ,p_cta_sector_a           IN OUT VARCHAR2
                          ,p_cta_sector_b           IN OUT VARCHAR2
                          ,p_cta_origen_garantia    IN OUT VARCHAR2
                          ,p_cta_origen_garantia_a  IN OUT VARCHAR2
                          ,p_cta_origen_garantia_b  IN OUT VARCHAR2
                          ,p_plazo_clase            IN OUT VARCHAR2 -- Caso 04032509
                          ,p_plazo_tipo             IN OUT VARCHAR2 -- Caso 04032509
                          ,p_cta_plazo_aut_a        IN OUT VARCHAR2 -- caso 05122709
                          ,p_cta_moneda             IN OUT VARCHAR2 --08102911. 20090520
                          ,p_cta_moneda_a           IN OUT VARCHAR2 --08102911. 20090520
                          ,p_cta_moneda_b           IN OUT VARCHAR2 --08102911. 20090520
                          )  IS
   -------------------------------------------------------------------------
   -- Retorna los codigos contables asociados a clase tipo y emisor
   -------------------------------------------------------------------------
    CURSOR c_cuentas_bursatilidad IS -- caso 05112515
           SELECT cbu_cuenta
                 ,cbu_cuenta_a
                 ,cbu_cuenta_b
                 ,DECODE(cbu_top_codigo,'*',0,1) * 10 + DECODE(cbu_cln_codigo,'*',0,1) * 1
               FROM PFCUENTAS_BURSATILIDAD_V2
               WHERE cbu_ref_codigo = p_tipo_movto
                     AND cbu_bur_codigo = p_bur_codigo
                     AND ((cbu_top_codigo = p_top_codigo AND cbu_cln_codigo = DECODE(cbu_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
                          (cbu_top_codigo = '*'          AND cbu_cln_codigo = DECODE(cbu_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
                          (cbu_top_codigo = p_top_codigo AND cbu_cln_codigo = '*') OR
                          (cbu_top_codigo = '*'          AND cbu_cln_codigo = '*')
              )
        ORDER BY 4 DESC
     ; --fin caso 05112515
     CURSOR c_cuentas_clase IS
       SELECT ctc_cuenta
             ,ctc_cuenta_a
             ,ctc_cuenta_b
             ,DECODE(ctc_top_codigo,'*',0,1) * 10 + DECODE(ctc_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_CLASE_V2
        WHERE ctc_ref_codigo = p_tipo_movto
          AND ctc_cin_codigo = p_clase
          AND ((ctc_top_codigo = p_top_codigo AND ctc_cln_codigo = DECODE(ctc_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (ctc_top_codigo = '*'          AND ctc_cln_codigo = DECODE(ctc_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (ctc_top_codigo = p_top_codigo AND ctc_cln_codigo = '*'                                                                                  ) OR
               (ctc_top_codigo = '*'          AND ctc_cln_codigo = '*'                                                                                  )
              )
        ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_tipo IS
       SELECT ctt_cuenta
             ,ctt_cuenta_a
             ,ctt_cuenta_b
             ,DECODE(ctt_top_codigo,'*',0,1) * 10 + DECODE(ctt_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_TIPO_V2
        WHERE ctt_ref_codigo = p_tipo_movto
          AND ctt_cin_codigo = p_clase
          AND ctt_tin_codigo = p_tipo
          AND ((ctt_top_codigo = p_top_codigo AND ctt_cln_codigo = DECODE(ctt_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (ctt_top_codigo = '*'          AND ctt_cln_codigo = DECODE(ctt_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (ctt_top_codigo = p_top_codigo AND ctt_cln_codigo = '*'                                                                                  ) OR
               (ctt_top_codigo = '*'          AND ctt_cln_codigo = '*'            )
              )
        ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_emisor IS
       SELECT cte_cuenta
             ,cte_cuenta_a
             ,cte_cuenta_b
             ,DECODE(cte_top_codigo,'*',0,1) * 10 + DECODE(cte_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_EMISOR_V2
        WHERE cte_ref_codigo = p_tipo_movto
          AND cte_emi_codigo = p_emisor
          AND ((cte_top_codigo = p_top_codigo AND cte_cln_codigo = DECODE(cte_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (cte_top_codigo = '*'          AND cte_cln_codigo = DECODE(cte_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (cte_top_codigo = p_top_codigo AND cte_cln_codigo = '*'                                                                                  ) OR
               (cte_top_codigo = '*'          AND cte_cln_codigo = '*'            )
              )
        ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_especie IS
       SELECT cts_cuenta
             ,cts_cuenta_a
             ,cts_cuenta_b
             ,DECODE(cts_top_codigo,'*',0,1) * 10 + DECODE(cts_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_ESPECIE_V2
        WHERE cts_ref_codigo = p_tipo_movto
          AND cts_cin_codigo = p_clase
          AND cts_tin_codigo = p_tipo
          AND cts_emi_codigo = p_emisor
          AND ((cts_top_codigo = p_top_codigo AND cts_cln_codigo = DECODE(cts_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (cts_top_codigo = '*'          AND cts_cln_codigo = DECODE(cts_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) OR
               (cts_top_codigo = p_top_codigo AND cts_cln_codigo = '*'                                                                                  ) OR
               (cts_top_codigo = '*'          AND cts_cln_codigo = '*')
              )
        ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_plazo IS
       SELECT cpl_cuenta
             ,cpl_cuenta_a
             ,cpl_cuenta_b
             ,DECODE(cpl_top_codigo,'*',0,1) * 1000 +
              DECODE(cpl_cln_codigo,'*',0,1) * 100  +
              DECODE(cpl_cin_codigo,'*',0,1) * 10   +
              DECODE(cpl_tin_codigo,'*',0,1) * 1
         FROM PFCUENTAS_PLAZO_V2
        WHERE cpl_pem_codigo = p_plazo
          AND cpl_ref_codigo = p_tipo_movto
          AND ((cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = p_plazo_tipo
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = p_plazo_clase   AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = p_top_codigo    AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = DECODE(cpl_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = '*'
               ) OR
               (cpl_cln_codigo = '*'             AND
                cpl_top_codigo = '*'             AND
                cpl_cin_codigo = '*'             AND
                cpl_tin_codigo = '*'
               )
              )
        ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_sector IS
       SELECT csc_cuenta
             ,csc_cuenta_a
             ,csc_cuenta_b
             ,DECODE(csc_top_codigo,'*',0,1) * 10 + DECODE(csc_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_SECTOR_V2
        WHERE csc_ref_codigo_sec  = DECODE(csc_clase_negocio_base,'P',p_sector_propio,p_sector_garantizado)
          AND csc_ref_codigo      = p_tipo_movto
          AND ((csc_cln_codigo    = DECODE(csc_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND csc_top_codigo     = p_top_codigo) OR
               (csc_cln_codigo    = '*'                                                                                   AND csc_top_codigo     = p_top_codigo) OR
               (csc_cln_codigo    = DECODE(csc_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND csc_top_codigo     = '*')          OR
               (csc_cln_codigo    = '*'                                                                                   AND csc_top_codigo     = '*')
              )
      ORDER BY 4 DESC
     ;
     CURSOR c_cuentas_sucursal IS
       SELECT sue_cuenta
         FROM PFSUCURSALES_EMPRESA_V2
        WHERE sue_emp_codigo = SUBSTR(p_sucursal,1,2)
          AND sue_codigo     = SUBSTR(p_sucursal,3)
     ;
     CURSOR c_cuentas_banco IS
       SELECT LTRIM(NVL(ref_valor,'99')) cuenta_banco
         FROM PFREFERENCIAS_V2
        WHERE ref_codigo     = p_banco
     ;
     CURSOR c_cuentas_origen_garantia IS
       SELECT cog_cuenta
             ,cog_cuenta_a
             ,cog_cuenta_b
             ,DECODE(cog_top_codigo,'*',0,1) * 10 + DECODE(cog_cln_codigo,'*',0,1) * 1
         FROM PFCUENTAS_ORIGEN_GARANTIA_V2
        WHERE cog_origen_garantia  = DECODE(cog_clase_negocio_base,'P',p_origen_titulo_propio,p_origen_titulo_garantizado)
          AND cog_ref_codigo       = p_tipo_movto
          AND ((cog_cln_codigo     = DECODE(cog_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND cog_top_codigo     = p_top_codigo) OR
               (cog_cln_codigo     = '*'                                                                                   AND cog_top_codigo     = p_top_codigo) OR
               (cog_cln_codigo     = DECODE(cog_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada) AND cog_top_codigo     = '*')          OR
               (cog_cln_codigo     = '*'                                                                                   AND cog_top_codigo     = '*')
              )
      ORDER BY 4 DESC
     ;
     --08102911. 20090520 Crear cursor de cuentas por moneda
          cursor c_cuentas_moneda is
            select ctm_cuenta
                  ,ctm_cuenta_a
                  ,ctm_cuenta_b
                  ,decode(ctm_top_codigo,'*',0,1) * 10 + decode(ctm_cln_codigo,'*',0,1) * 1
              from pfcuentas_moneda_v2
             where ctm_ref_codigo = p_tipo_movto
               and ctm_cin_codigo = p_clase
               and ctm_tin_codigo = p_tipo
               and ctm_emi_codigo = p_emisor
               and ((ctm_top_codigo = p_top_codigo and ctm_cln_codigo = decode(ctm_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) or
                    (ctm_top_codigo = '*'          and ctm_cln_codigo = decode(ctm_clase_negocio_base,'P',p_clase_negocio_propia,p_clase_negocio_garantizada)) or
                    (ctm_top_codigo = p_top_codigo and ctm_cln_codigo = '*'                                                                                  ) or
                    (ctm_top_codigo = '*'          and ctm_cln_codigo = '*')
                   )
             order by 4 desc
     ;
   BEGIN  -- de pr_subcuentas
     -- Bursatilidades
     p_cta_bur    := NULL;
     p_cta_bur_a  := NULL;
     p_cta_bur_b  := NULL;
     FOR icb IN c_cuentas_bursatilidad LOOP
       p_cta_bur    := icb.cbu_cuenta;
       p_cta_bur_a  := icb.cbu_cuenta_a;
       p_cta_bur_b  := icb.cbu_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Clase de Inversi¿n
     p_cta_clase    := NULL;
     p_cta_clase_a  := NULL;
     p_cta_clase_b  := NULL;
     FOR icc IN c_cuentas_clase LOOP
       p_cta_clase    := icc.ctc_cuenta;
       p_cta_clase_a  := icc.ctc_cuenta_a;
       p_cta_clase_b  := icc.ctc_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Tipo de Inversi¿n
     p_cta_tipo    := NULL;
     p_cta_tipo_a  := NULL;
     p_cta_tipo_b  := NULL;
     FOR ict IN c_cuentas_tipo LOOP
       p_cta_tipo    := ict.ctt_cuenta;
       p_cta_tipo_a  := ict.ctt_cuenta_a;
       p_cta_tipo_b  := ict.ctt_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Emisor
     p_cta_emisor    := NULL;
     p_cta_emisor_a  := NULL;
     p_cta_emisor_b  := NULL;
     FOR ice IN c_cuentas_emisor LOOP
       p_cta_emisor    := ice.cte_cuenta;
       p_cta_emisor_a  := ice.cte_cuenta_a;
       p_cta_emisor_b  := ice.cte_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Especie
     p_cta_especie    := NULL;
     p_cta_especie_a  := NULL;
     p_cta_especie_b  := NULL;
     FOR ice IN c_cuentas_especie LOOP
       p_cta_especie    := ice.cts_cuenta;
       p_cta_especie_a  := ice.cts_cuenta_a;
       p_cta_especie_b  := ice.cts_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Plazo
     p_cta_plazo    := NULL;
     p_cta_plazo_a  := NULL;
     p_cta_plazo_b  := NULL;
     FOR icp IN c_cuentas_plazo LOOP
       p_cta_plazo    := icp.cpl_cuenta;
       p_cta_plazo_a  := icp.cpl_cuenta_a;
       p_cta_plazo_b  := icp.cpl_cuenta_b;
       EXIT;
     END LOOP;
     -- Cuentas por Sector
     p_cta_sector    := NULL;
     p_cta_sector_a  := NULL;
     p_cta_sector_b  := NULL;
     FOR ics IN c_cuentas_sector LOOP
       p_cta_sector    := ics.csc_cuenta;
       p_cta_sector_a  := ics.csc_cuenta_a;
       p_cta_sector_b  := ics.csc_cuenta_b;
       EXIT;
     END LOOP;
    -- Cuentas por Sucursal
     p_cta_sucursal    := NULL;
     FOR ics IN c_cuentas_sucursal LOOP
       p_cta_sucursal := ics.sue_cuenta;
       EXIT;
     END LOOP;
    -- Cuentas por Banco
     p_cta_cod_banco    := NULL;
     FOR icb IN c_cuentas_banco LOOP
       p_cta_cod_banco := icb.cuenta_banco;
       EXIT;
     END LOOP;
     -- Cuentas por Origen Garantia
     p_cta_origen_garantia    := NULL;
     p_cta_origen_garantia_a  := NULL;
     p_cta_origen_garantia_b  := NULL;
     FOR ico IN c_cuentas_origen_garantia LOOP
       p_cta_origen_garantia    := ico.cog_cuenta;
       p_cta_origen_garantia_a  := ico.cog_cuenta_a;
       p_cta_origen_garantia_b  := ico.cog_cuenta_b;
       EXIT;
     END LOOP;
     -- Codigo contable de autoretencion
     p_cta_plazo_aut :=  Fbd_Porcentaje_Autoretencion(p_plazo_facial,p_emisor,p_moneda,'C','NA');
     p_cta_plazo_aut_a :=  Fbd_Porcentaje_Autoretencion(p_plazo_facial,p_emisor,p_moneda,'A','NA');
      --08102911. 20090520. Cuentas por moneda
          p_cta_moneda    := NULL;
          p_cta_moneda_a  := NULL;
          p_cta_moneda_b  := NULL;
          for icm in c_cuentas_moneda loop
            p_cta_moneda    := icm.ctm_cuenta;
            p_cta_moneda_a  := icm.ctm_cuenta_a;
            p_cta_moneda_b  := icm.ctm_cuenta_b;
            exit;
          end loop;
   END pr_subcuentas;
   -------------------------------------------------------------------------
   FUNCTION fn_cuenta(p_cuenta         IN OUT VARCHAR2
                     ,p_clase                 VARCHAR2
                     ,p_clase_a               VARCHAR2
                     ,p_clase_b              VARCHAR2
                     ,p_tipo                  VARCHAR2
                     ,p_tipo_a                VARCHAR2
                     ,p_tipo_b                VARCHAR2
                     ,p_emisor                VARCHAR2
                     ,p_emisor_a              VARCHAR2
                     ,p_emisor_b              VARCHAR2
                     ,p_plazo                 VARCHAR2
                     ,p_plazo_a               VARCHAR2
                     ,p_plazo_b               VARCHAR2
                     ,p_especie               VARCHAR2
                     ,p_especie_a             VARCHAR2
                     ,p_especie_b             VARCHAR2
                     ,p_cta_sucursal          VARCHAR2
                     ,p_cta_cod_banco         VARCHAR2
                     ,p_cta_plazo_aut         VARCHAR2   -- Plazo autoretencion
                     ,p_bur                   VARCHAR2   -- cta Bursatilidad
                     ,p_bur_a                 VARCHAR2   -- cta a Bursatilidad
                     ,p_bur_b                 VARCHAR2   -- cta a Bursatilidad adicional
                     ,p_riesgo                VARCHAR2   -- cta riesgo
                     ,p_riesgo_a              VARCHAR2   -- cta a riesgo
                     ,p_cta_sector            VARCHAR2   -- cta sector alterno   caso 03091210
                     ,p_cta_sector_a          VARCHAR2   -- cta a sector alterno caso 03091210
                     ,p_cta_sector_b          VARCHAR2   -- cta a sector alterno adicional
                     ,p_cta_origen_garantia   VARCHAR2   -- cta origen garantia (G ¿ P)
                     ,p_cta_origen_garantia_a VARCHAR2   -- cta alterna origen garantia (G ¿ P)
                     ,p_cta_origen_garantia_b VARCHAR2   -- cta alterna adicional origen garantia (G ¿ P)
                     ,p_cta_plazo_aut_a       VARCHAR2   -- Cta Alterna Plazo autoretencion
                     ,p_cta_moneda            VARCHAR2   --08102911. 20090520
                     ,p_cta_moneda_a          VARCHAR2   --08102911. 20090520
                     ,p_cta_moneda_b          VARCHAR2   --08102911. 20090520
                     ) RETURN NUMBER IS
   -------------------------------------------------------------------------
   -- Genera el codigo de la cuenta afectada por una operacion
   -- Toma la cadena p_cuenta y reemplaza
   --     #A   por tipo de bursatilidad
   --     #AA  por tipo de bursatilidad alterno
   --     #AB  por tipo de bursatilidad alterno adicional
   --     #G   por tipo de riesgo
   --     #GA  por tipo de riesgo alterno
   --     #T   por tipo de inversion
   --     #TA  por tipo de inversion alterno
   --     #TB  por tipo de inversion alterno adicional
   --     #E   por emisor
   --     #EA  por emisor alterno
   --     #EB  por emisor alterno adicional
   --     #C   por clase de inversion
   --     #CA  por clase de inversion alterno
   --     #CB  por clase de inversion alterno adicional
   --     #L   por codigo contable correspondiente al plazo
   --     #LA  por codigo contable correspondiente al plazo alterno
   --     #LB  por codigo contable correspondiente al plazo alterno adicional
   --     #S   por especie
   --     #SA  por especie alterna
   --     #SB  por especie alterna adicional
   --     #D   por codigo contable de la sucursal (Aplica solo divisas)
   --     #O   por codigo del banco (Ref_valor de cada banco segun cumplimiento financiero)
   --     #ZA  por cuenta alterna asociada al plazo de autoretencion seg¿n emisor y moneda.
   --     #Z   por cuenta asociada al plazo de autoretencion seg¿n emisor y moneda.
   --     #Y   por codigo contable sector alterno (pfpersonas_v2.per_ref_codigo_sec)
   --     #YA  por codigo contable alterno sector alterno
   --     #YB  por codigo contable alterno sector alterno adicional
   --     #I   por codigo contable origen del t¿tulo dado/recibido en garantia
   --     #IA  por codigo contable alterno origen del t¿tulo dado/recibido en garantia
   --     #IB  por codigo contable alterno adicional origen del t¿tulo dado/recibido en garantia
   --     #IB  ......
   --     #M   por moneda                     --08102911. 20090520
   --     #MA  por moneda alterna             --08102911. 20090520
   --     #MB  por moneda alterna adicional   --08102911. 20090520
   -------------------------------------------------------------------------
      -------------------------------------------------------------------------
      -- Definicion variables
      -------------------------------------------------------------------------
      t_cuenta VARCHAR2(16);
   -------------------------------------------------------------------------
   BEGIN  -- de fn_cuenta
     t_cuenta := p_cuenta;
     SELECT REPLACE(t_cuenta,'#CB' ,p_clase_b               ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#CA' ,p_clase_a               ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#C'  ,p_clase                 ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#TB' ,p_tipo_b                ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#TA' ,p_tipo_a                ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#T'  ,p_tipo                  ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#EB' ,p_emisor_b              ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#EA' ,p_emisor_a              ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#E'  ,p_emisor                ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#LB' ,p_plazo_b               ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#LA' ,p_plazo_a               ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#L'  ,p_plazo                 ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#SB' ,p_especie_b             ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#SA' ,p_especie_a             ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#S'  ,p_especie               ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#D'  ,p_cta_sucursal          ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#O'  ,p_cta_cod_banco         ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#ZA' ,p_cta_plazo_aut_a       ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#Z'  ,p_cta_plazo_aut         ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#AB' ,p_bur_b                 ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#AA' ,p_bur_a                 ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#A'  ,p_bur                   ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#GA' ,p_riesgo_a              ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#G'  ,p_riesgo                ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#YB' ,p_cta_sector_b          ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#YA' ,p_cta_sector_a          ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#Y'  ,p_cta_sector            ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#IB' ,p_cta_origen_garantia_b ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#IA' ,p_cta_origen_garantia_a ) INTO t_cuenta FROM dual;
     SELECT REPLACE(t_cuenta,'#I'  ,p_cta_origen_garantia   ) INTO t_cuenta FROM dual;
     --20090618.ACG. Cambiar el orden de evaluacion de las siguientes lineas
          --se evaluan en el siguiente orden #MB, #MA y #M
          --antes estaba evaluadas asi #M, #MA y #MB
     SELECT REPLACE(t_cuenta,'#MB' ,p_cta_moneda_b          ) INTO t_cuenta FROM dual;       --08102911. 20090520
     SELECT REPLACE(t_cuenta,'#MA' ,p_cta_moneda_a          ) INTO t_cuenta FROM dual;       --08102911. 20090520
     SELECT REPLACE(t_cuenta,'#M'  ,p_cta_moneda            ) INTO t_cuenta FROM dual;       --08102911. 20090520
     p_cuenta := t_cuenta;
     RETURN(0);
   EXCEPTION WHEN OTHERS THEN RETURN(SQLCODE);
   END fn_cuenta;
   -------------------------------------------------------------------------
   FUNCTION fn_descripcion(p_especie            VARCHAR2
                                ,p_tipo_operacion     VARCHAR2
                                ,p_unidad             VARCHAR2
                                ,p_valor              NUMBER
                                ,p_precio             NUMBER
                                ,p_tasa               VARCHAR2
                                ,p_clase              VARCHAR2
                                ,p_mercado            VARCHAR2
                                ,p_descripcion IN OUT VARCHAR2
                                ,pfd_fecha            DATE
                                ,pfd_emp_codigo       VARCHAR2
                                ,p_activo_pasivo      VARCHAR2
                                 ) RETURN NUMBER IS
   -------------------------------------------------------------------------
   -- Genera la descripcion del movimiento contable segun el tipo de oper.
   -- ICA 971104: cambio de la descripcion de los movimientos contables
   -- se quitan el valor de la operacion, la tasa efectiva y el mercado
   -- se pone la tasa nominal y la modalidad
   -- w_valor llega segun parametrizacion (moneda o $).
   -------------------------------------------------------------------------
     t_operacion   PFTIPO_OPERACIONES_V2.top_descripcion%TYPE ;
     t_precio      NUMBER;
   -------------------------------------------------------------------------
   BEGIN  -- de fn_descripcion
     SELECT DECODE(p_activo_pasivo,'E',RTRIM(top_desc_pasivo)
                                      ,RTRIM(top_descripcion))  INTO t_operacion
       FROM PFTIPO_OPERACIONES_V2
      WHERE top_codigo = p_tipo_operacion;
     IF p_unidad <> Fbd_Emp_Unidad(pfd_emp_codigo) THEN
       t_precio      := Fbd_Unidad_En_Pesos(p_unidad,pfd_fecha,Fbd_Emp_Unidad(pfd_emp_codigo));
       p_descripcion :=  t_operacion||' '                        -- Compra
                       ||LTRIM(TO_CHAR((p_valor),'999,999,999,999.99'))      -- 10000
                       ||' '||p_especie                          -- acciones xx
                       ||' '                                     -- a
                       ||p_unidad||' : '                         -- US$
                       ||LTRIM(TO_CHAR(t_precio,'99,999.99'))    -- 50
                       ||''                                      -- c/u
                       ;
     ELSE
       p_descripcion := t_operacion                              -- Compra
                       ||' '||p_especie                          -- bonos
                       ;
     END IF;
     IF p_clase = 'ZFI' THEN
       p_descripcion := p_descripcion                             -- ...
                        ||' al '                                  -- al
                        || p_tasa                                 -- 32.00 SV
                        ;
       p_descripcion := SUBSTR(p_descripcion,1,70);
     END IF;
   RETURN(0);
   EXCEPTION WHEN OTHERS THEN RETURN(SQLCODE);
   END fn_descripcion;
   -------------------------------------------------------------------------
   FUNCTION fn_busque_valor(pfb_operacion     NUMBER
                           ,pfb_tipo_concepto VARCHAR2
                           ,pfb_concepto      VARCHAR2
                           ,pfb_tipo_valor    VARCHAR2
                           ) RETURN NUMBER IS
   -------------------------------------------------------------------------
   -- Busca si ya se calculo el valor de un concepto para una operacion, si
   -- es asi lo retirna, sino retorna nulo.
   -------------------------------------------------------------------------
   BEGIN  -- de fn_busque_valor
     FOR i IN 1..max_va LOOP
       IF va_operacion    (i) = pfb_operacion     AND
          va_tipo_concepto(i) = pfb_tipo_concepto AND
          va_concepto     (i) = pfb_concepto      AND
          va_tipo_valor   (i) = pfb_tipo_valor    THEN
            RETURN(va_valor(i));
       END IF;
     END LOOP;
   -- Si no encuentra retone nulo.
   RETURN(NULL);
   EXCEPTION WHEN OTHERS THEN RETURN(NULL);
   END fn_busque_valor;
   -------------------------------------------------------------------------
   PROCEDURE pr_guarde_valor(ppg_operacion     NUMBER
                            ,ppg_tipo_concepto VARCHAR2
                            ,ppg_concepto      VARCHAR2
                            ,ppg_tipo_valor    VARCHAR2
                            ,ppg_valor         NUMBER
                            ) IS
   -------------------------------------------------------------------------
   -- Busca si ya se calculo el valor de un concepto para una operacion, si
   -- es asi lo retirna, sino retorna nulo.
   -------------------------------------------------------------------------
   BEGIN  -- de pr_guarde_valor
     max_va := max_va + 1;
     va_operacion    (max_va) := ppg_operacion;
     va_tipo_concepto(max_va) := ppg_tipo_concepto;
     va_concepto     (max_va) := ppg_concepto;
     va_tipo_valor   (max_va) := ppg_tipo_valor;
     va_valor        (max_va) := ppg_valor;
   END pr_guarde_valor;
   -------------------------------------------------------------------------
   FUNCTION fn_benef_hist(pfb_titulo NUMBER
                            ,pfb_fecha  DATE
                            ) RETURN NUMBER IS
      t_benef PFPERSONAS_V2.per_ident%TYPE;
      -------------------------------------------------------------------------
      -- Retorna el nit del primer beneficiario vigente que encuentra para
      -- el titulo a la fecha de parametro -- 20050620
      -------------------------------------------------------------------------
      BEGIN
        BEGIN
           SELECT bxt_per_ident
             INTO t_benef
             FROM PFBENEFICIARIOS_TITULO_V2
            WHERE bxt_consec = (SELECT MAX(bxt_consec)
                                  FROM PFBENEFICIARIOS_TITULO_V2
                                 WHERE (bxt_f_inicial <= pfb_fecha
                                      AND (bxt_f_final IS NULL AND bxt_estado = 'iAC') --GPQ 20040318
                                      AND bxt_tit_consec = pfb_titulo
                                     )
                                 OR  (bxt_f_inicial <= pfb_fecha
                                      AND (bxt_f_final = pfb_fecha                     --GPQ 20040318
                                           AND bxt_estado <> 'iAC'                     --GPQ 20040318
                                          )
                                      AND bxt_tit_consec = pfb_titulo
                                     )
                              )
             AND bxt_tit_consec = pfb_titulo
            ;
        EXCEPTION WHEN NO_DATA_FOUND THEN t_benef := Fbd_Beneficiario(pfb_titulo);
        END;
        RETURN (t_benef);
      END fn_benef_hist;
  -------------------------------------------------------------------------------
BEGIN -- De fbd_movto_contable
  -- Inicio del programa principal
  savepoint sp_movto_contable;
  -- Revisar si la referencia DNB indica en que anualidad obtener el plazo:
  begin
    SELECT NVL(ref_valor,'xxx')
      INTO t_anualidad_nro_l
      FROM PFREFERENCIAS_V2
     WHERE ref_codigo = 'DNB';
  EXCEPTION WHEN OTHERS THEN
    t_anualidad_nro_l := 'xxx';
  END;
  -- Recorrido al cursor operaciones
  for mo in c_mvto_operaciones loop
    glo_titulo  := mo.titulo;
    pr_mensaje ('****************************************************************************************************************');
    pr_mensaje ('Titulo '||glo_titulo||'    Operacion '||mo.operacion);
    --
    IF mo.inversion <> glo_inv THEN
      IF mo.clase = 'ZFI' THEN glo_tasa_nominal   := Fbd_Facial(mo.inversion)||' '||Fbd_Mod_Desc_Corta(mo.inversion);
      ELSE                     glo_tasa_nominal   := 'No Aplica'             ||' '||'NAp';
      END IF;
      glo_clasificacion  := Fbd_Desc_Especie_Tit(mo.titulo,'CLASIFICACION');
      IF mo.por_activo_pasivo = 'D' THEN glo_modalidad      := Fbd_Valor_Opcion('T',glo_titulo);
      ELSE
        IF mo.clase = 'ZFI' THEN glo_modalidad      := Fbd_Modalidad(mo.inversion);
        ELSE                     glo_modalidad      := '*';
        END IF;
      END IF;
      glo_inv           := mo.inversion;
    eND IF;
    -- Asignar las globales.
    mo.tasa_nominal   := glo_tasa_nominal;
    mo.clasificacion  := glo_clasificacion;
    mo.modalidad      := glo_modalidad;
    --
    IF mo.titulo <> glo_tit THEN
      glo_beneficiario := Fbd_Beneficiario(mo.titulo);
      glo_tit         := mo.titulo;
    END IF;
    IF mo.por_activo_pasivo = 'D' THEN
      glo_garantia     := Fbd_Oper_Contabiliza_Div(mo.inv_tipo_operacion);
    ELSE
      glo_garantia := Fbd_Garantia_Div(mo.titulo,pfm_fecha,mo.tipo_operacion);
      --IF glo_garantia = 'N' THEN glo_garantia := Fbd_Garantia(mo.titulo,pfm_fecha,mo.tipo_operacion);      END IF;  -- ORC 20071205
      IF glo_garantia = 'N' and fbd_garantia(mo.titulo,pfm_fecha,mo.tipo_operacion) = 'R' and mo.tipo_operacion = 'F' THEN -- HDV 08070827
        select count(*)
          into t_cuantos_cont
          from pfestados_titulo_v2
         where est_tit_f_estado <= pfm_fecha
           and est_tit_estado   in ('TPR','TRE')
           and est_tit_consec    = mo.titulo;
        if t_cuantos_cont = 0 then
          glo_garantia := fbd_garantia(mo.titulo,pfm_fecha,mo.tipo_operacion);
        end if;
      elsif glo_garantia <> 'D' then
        glo_garantia := fbd_garantia(mo.titulo,pfm_fecha,mo.tipo_operacion);
      END IF;  -- ORC 20071205
    END IF;
    mo.beneficiario := glo_beneficiario;
    mo.garantia     := glo_garantia;
    -- Traer la cantidad de decimales a redondear el movimiento contable
    begin
      select NVL(cnv_decimales,2)
        into glo_round
        from PFCONVERSIONES_V2
       where cnv_codigo = mo.unidad;
    exception
      when no_data_found then
        t_mensaje_error := 'No existe una unidad (moneda) con tipo '||mo.unidad||' buscando decimales;';
        raise t_error_controlado;      -- 12041322 Patch PP20831
      when others then
        t_mensaje_error := 'Error buscando decimales para la unidad (moneda) '||mo.unidad;
        raise t_error_controlado;      -- 12041322 Patch PP20831
    end;
    -- Para portafolios de emision el plazo siempre sera el facial.
    IF mo.activo_pasivo = 'E' THEN
      t_activo_pasivo := 'E';
      IF t_anualidad_nro_l IN ('360','365') THEN
        IF Fbd_Cliente = 'o16' THEN t_plazo := Fbd_Plazo(mo.inv_f_vcto,mo.inv_f_compra ,t_anualidad_nro_l);
        ELSE                        t_plazo := Fbd_Plazo(mo.inv_f_vcto,mo.inv_f_emision,t_anualidad_nro_l);
        END IF;
      ELSE
        -- Si el cliente es Bancoldex el plazo va entre compra y vcto. Para los demas clientes va contra el plazo de la inversi¿n.
        IF Fbd_Cliente = 'o16' THEN t_plazo := mo.plazo_vcto_comp;
        ELSE                        t_plazo := mo.plazo_facial;
        END IF;
      END IF;
    ELSE
      -- JCD. Caso 04032509 Si la operacion esta relacionado con forward de RF
      -- debe buscar el plazo en la operacion correspondiente
      IF mo.tipo_operacion IN ('(','N','P','R',')','O','Q','S') AND mo.clase = 'ZFI' THEN
        --begin
          SELECT Fbd_Dias_Habiles(ope_f_compromiso,ope_fecha,ope_emp_codigo)
            INTO t_plazo
            FROM PFOPERACIONES_V2
           WHERE ope_top_codigo = DECODE(mo.tipo_operacion
                                         ,'(','1'
                                         ,'N','1'
                                         ,'P','1'
                                         ,'R','1'
                                         ,')','2'
                                         ,'O','2'
                                         ,'Q','2'
                                         ,'S','2'
                                        )
             AND ope_tit_consec = mo.titulo;
        --exception
          --
        --end;
        t_activo_pasivo := 'F';
      ELSE
        t_plazo         := mo.plazo;
        t_activo_pasivo := 'E';
      END IF;
    END IF;
    --
    begin
      t_pla_clase_inv := mo.clase_inversion;
      t_pla_tipo_inv  := mo.tipo_inversion;
      t_cod_plazo     := '';
      pn_plazo(t_pla_clase_inv
              ,t_pla_tipo_inv
              ,t_plazo
              ,t_cod_plazo
              ,t_activo_pasivo
              );
      pr_mensaje ('mo.descripcion              '||mo.descripcion);
      pr_mensaje ('mo.tipo_movto               '||mo.tipo_movto );
      pr_mensaje ('mo.clase_inversion          '||mo.clase_inversion);
      pr_mensaje ('mo.tipo_inversion           '||mo.tipo_inversion );
      pr_mensaje ('mo.emisor                   '||mo.emisor);
      pr_mensaje ('mo.fuente                   '||mo.fuente);
      pr_mensaje ('t_cod_plazo                 '||t_cod_plazo);
      pr_mensaje ('mo.tipo_operacion           '||mo.tipo_operacion);
      pr_mensaje ('substr(mo.banco,1,3)        '||SUBSTR(mo.banco,1,3));
      pr_mensaje ('mo.unidad                   '||mo.unidad);
      pr_mensaje ('mo.plazo_facial             '||mo.plazo_facial);
      pr_mensaje ('mo.bursatilidad             '||mo.bursatilidad);
      pr_mensaje ('mo.sector_propio            '||mo.sector_propio);
      pr_mensaje ('p_sector_propio             '||mo.sector_propio             );
      pr_mensaje ('p_sector_garantizado        '||mo.sector_garantizado        );
      pr_mensaje ('p_clase_negocio_propia      '||mo.clase_negocio_propia      );
      pr_mensaje ('p_clase_negocio_garantizada '||mo.clase_negocio_garantizada );
      pr_mensaje ('p_origen_titulo_propio      '||mo.origen_titulo_propio      );
      pr_mensaje ('p_origen_titulo_garantizado '||mo.origen_titulo_garantizado);
      pr_mensaje ('mo.sector_garantizado       '||mo.sector_garantizado);
      --
      pr_subcuentas(mo.tipo_movto
                   ,mo.clase_inversion
                   ,mo.tipo_inversion
                   ,mo.emisor
                   ,t_cod_plazo
                   ,mo.tipo_operacion
                   ,mo.empresa||NVL(mo.sucursal,'xxx')
                   ,SUBSTR(mo.banco,1,3)
                   ,mo.unidad
                   ,mo.plazo_facial
                   ,mo.bursatilidad --Codigo bursatilidad
                   ,mo.sector_propio
                   ,mo.sector_garantizado
                   ,mo.clase_negocio_propia       -- Aqui va la propia
                   ,mo.clase_negocio_garantizada  -- Aqui va la garantizada.
                   ,mo.origen_titulo_propio
                   ,mo.origen_titulo_garantizado
                   ,t_cta_clase_inversion
                   ,t_cta_clase_inversion_a
                   ,t_cta_clase_inversion_b
                   ,t_cta_tipo_inversion
                   ,t_cta_tipo_inversion_a
                   ,t_cta_tipo_inversion_b
                   ,t_cta_emisor
                   ,t_cta_emisor_a
                   ,t_cta_emisor_b
                   ,t_cta_plazo
                   ,t_cta_plazo_a
                   ,t_cta_plazo_b
                   ,t_cta_especie
                   ,t_cta_especie_a
                   ,t_cta_especie_b
                   ,t_cta_sucursal
                   ,t_cta_cod_banco
                   ,t_cta_plazo_aut
                   ,t_cta_bur -- Cuenta asociada a Burs
                   ,t_cta_bur_a -- Cuenta A asociada a Burs
                   ,t_cta_bur_b -- Cuenta A asociada a Burs
                   ,t_cta_sector
                   ,t_cta_sector_a
                   ,t_cta_sector_b
                   ,t_cta_origen_garantia
                   ,t_cta_origen_garantia_a
                   ,t_cta_origen_garantia_b
                   ,t_pla_clase_inv                     -- caso 04032509
                   ,t_pla_tipo_inv                      -- caso 04032509
                   ,t_cta_plazo_aut_a                   -- caso 05122709
                   ,t_cta_moneda                        --08102911. 20090520
                   ,t_cta_moneda_a                      --08102911. 20090520
                   ,t_cta_moneda_b                      --08102911. 20090520
                   );
      pr_mensaje ('t_cta_moneda                '||t_cta_moneda);           --08102911. 20090520
      pr_mensaje ('t_cta_moneda_a              '||t_cta_moneda_a);         --08102911. 20090520
      pr_mensaje ('t_cta_moneda_b              '||t_cta_moneda_b);         --08102911. 20090520
      t_cta_riesgo    := Fbd_Calificacion_Titulo(mo.titulo, pfm_fecha,'CUENTA');
      t_cta_riesgo_a  := Fbd_Calificacion_Titulo(mo.titulo, pfm_fecha,'CUENTA_A');
      -- Inicio 11072508
      begin
        select nvl(por_fecha_cierre,pfm_fecha)
          into t_fecha_cierre
          from pfportafolios_v2
         where por_emp_codigo = pfm_empresa
           and por_codigo     = pfm_portafolio;
      exception
        when no_data_found then
          t_mensaje_error := 'No existe una empresa y portafolio con el código '||pfm_empresa||' - '||pfm_portafolio;
          raise t_error_controlado;
        when others then
          t_mensaje_error := 'Error buscando la fecha de cierre para el portafolio '||pfm_empresa||' - '||pfm_portafolio||' ['||
                             dbms_utility.format_error_stack||dbms_utility.format_error_backtrace||']';
          raise t_error_controlado;
      end;
      t_cta_riesgo_ayer    := Fbd_Calificacion_Titulo(mo.titulo, t_fecha_cierre,'CUENTA');
      t_cta_riesgo_a_ayer  := Fbd_Calificacion_Titulo(mo.titulo, t_fecha_cierre,'CUENTA_A');
      -- Fin 11072508
      IF mo.modalidad = '*' THEN w_modalidad := 'V';
      ELSE                       w_modalidad := mo.modalidad;
      END IF;
      w_clasificacion  := mo.clasificacion;
      t_fuente         := mo.fuente  ;   -- Asignacion se hace para buscar
      pr_mensaje('t_fuente='||t_fuente);
      t_modalidad      := w_modalidad;   -- el concepto por tipo generador
      t_mercado        := mo.mercado ;   -- de las cuentas
      t_clase_negocio      := mo.clase_negocio;  -- ORC 20071027
      t_clase_negocio_liq  := mo.clase_negocio_liq;  -- ORC 20071205
      t_clase_negocio_ori  := mo.clase_negocio_ori;  -- JCD 20080402

      --08013117. t_clase_negocio_liq viene con la ultima clase neg vigente
      --Pero cuando en la misma fecha y para el mismo titulo se presentan las operaciones
      --U Dar en Garantia(P) y I Recibir Garantia(A)
      --W Liberar Garantia(P) o T Liberar Garantia(A).
      --se debe traer la clase de negocio correspondiente a cada operacion, teniendo en cuenta si es pasivo o Activo
      --08020706.Cambiar * por t_clase_negocio_liq

      IF mo.tipo_operacion IN ('T','I') THEN
        t_clase_negocio_liq := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO','A'),t_clase_negocio_liq);
        t_clase_negocio_ori := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO_ORI','A'),t_clase_negocio_ori);  --Caso 08091005
      ELSIF mo.tipo_operacion IN ('W','U') THEN
        t_clase_negocio_liq := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO','P'),t_clase_negocio_liq);
        t_clase_negocio_ori := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO_ORI','P'),t_clase_negocio_ori);  --Caso 08091005
      ELSIF mo.tipo_operacion IN ('F') THEN  --Caso 08091005
        t_clase_negocio_liq := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO','*'),t_clase_negocio_liq);             --Caso 08091005
        t_clase_negocio_ori := NVL(Fbd_Operacion_Garantizada(mo.titulo,pfm_fecha,'CLASE_NEGOCIO_ORI','*'),t_clase_negocio_ori);         --Caso 08091005
      END IF;
      -- Traer el concepto por tipo de operacion asociado
                -- Traer las respectivas cuentas
      pr_mensaje('Título '||mo.titulo||'  Operacion '||mo.operacion);
      pr_mensaje('Busca un encabezado de cuentas con la sgte información: ');
      pr_mensaje('Tipo Movto:'||' '||mo.tipo_movto||' '||'Clasif.:'||' '||w_clasificacion||' '||'Tipo Oper:'||' '||mo.tipo_operacion||' '||
                 'Fte/Prop:'||' '||t_fuente||' '||'Mod:'||' '||t_modalidad||' '||'Merc:'||' '||t_mercado||' '||'Act/Pas:'||' '||mo.activo_pasivo||' '||
                 'Gtia:'||' '||mo.garantia||' '||'Calculo: '||' '||mo.calculo||' '||'Ref.Con: '||mo.ref_contable||'  Clase Negocio: '||t_clase_negocio||'  Clase Negocio Liq: '||t_clase_negocio_liq||'  Clase Negocio Ori: '||t_clase_negocio_ori);  -- ORC 20071205 -- JCD 20080402

      pr_ct_oper(mo.tipo_movto,mo.tipo_operacion,w_clasificacion,t_fuente,t_mercado,t_modalidad,mo.activo_pasivo,mo.garantia,mo.ref_contable,t_clase_negocio,t_clase_negocio_liq,t_clase_negocio_ori); -- ORC 20071027  -- ORC 20071205 -- JCD 20080402
      -- Traer las respectivas cuentas
      pr_mensaje('La soluci¿n de asteriscos, retorna la siguiente informaci¿n: (con la cual se buscan cuentas)');
      pr_mensaje('Tipo Movto:'||' '||mo.tipo_movto||' '||'Clasif.:'||' '||w_clasificacion||' '||'Tipo Oper:'||' '||mo.tipo_operacion||' '||
                 'Fte/Prop:'||' '||t_fuente||' '||'Mod:'||' '||t_modalidad||' '||'Merc:'||' '||t_mercado||' '||'Act/Pas:'||' '||mo.activo_pasivo||' '||
                 'Gtia:'||' '||mo.garantia||' '||'Calculo: '||' '||mo.calculo||' '||'Ref.Con: '||mo.ref_contable||'  Clase Negocio: '||t_clase_negocio||'  Clase Negocio Liq: '||t_clase_negocio_liq||'  Clase Negocio Ori: '||t_clase_negocio_ori);  -- ORC 20071205 -- JCD 20080402
      --
      IF t_fuente IS NOT NULL THEN
        t_debitos  := 0;
        t_creditos := 0;
        pr_mensaje('antes del cursor de cuentas:'||mo.tipo_movto||','||w_clasificacion||','||mo.tipo_operacion||','||t_fuente||','||t_modalidad||','||t_mercado||','||mo.activo_pasivo||','||mo.garantia||','||mo.ref_contable||','||t_clase_negocio||','||t_clase_negocio_liq||','||t_clase_negocio_ori);
        FOR cc IN c_cuentas(mo.tipo_movto,w_clasificacion,mo.tipo_operacion,t_fuente,t_modalidad,t_mercado,mo.activo_pasivo,mo.garantia,mo.ref_contable,t_clase_negocio,t_clase_negocio_liq,t_clase_negocio_ori) LOOP -- ORC 20071205 -- JCD 20080402
          pr_mensaje('----------------------------------------------------------------------------------------------------------------------------------------');
          pr_mensaje('Encuentra la cuenta '||cc.cuenta||' para el T¿tulo='||mo.titulo||' Oper='||mo.operacion||' '||'tipo concepto '||cc.tipo_concepto||' '||'Concepto '||cc.concepto||' '||
                     'Tipo Valor '||cc.tipo_valor||' Fecha '||pfm_fecha
                    );

          ----
          -- Si Swap, busque en pfasociacion_conceptos_do_v2 si se indica que la busueda de metacaracteres se hace para la obligaci¿n:
          If mo.c_swap = 'S' and mo.obligacion is not null then
            t_der_obl := 'D';
            for icd in (select decode(aco_top_codigo,'*',0,1) peso
                              ,aco_tipo                       tipo
                          from pfasociacion_conceptos_do_v2
                         where aco_ref_codigo  = mo.tipo_movto
                           and (aco_top_codigo = mo.tipo_operacion or aco_top_codigo = '*')
                           and aco_concepto    = cc.concepto
                         order by 1 desc
                       )
            loop
              t_der_obl := icd.tipo;
              exit;
            end loop;

            for icd in (select tit_cin_codigo                 clase_inversion
                              ,tit_tin_codigo                 tipo_inversion
                              ,tit_emi_codigo                 emisor
                              ,inv_cnv_codigo                 unidad
                          from pfinversiones_v2
                              ,pftitulos_v2
                         where inv_consec      = tit_inv_consec
                           and tit_consec      = decode(t_der_obl,'D',mo.titulo,mo.obligacion)
                       )
            loop
              pr_subcuentas(mo.tipo_movto
                           ,icd.clase_inversion
                           ,icd.tipo_inversion
                           ,icd.emisor
                           ,t_cod_plazo
                           ,mo.tipo_operacion
                           ,mo.empresa||NVL(mo.sucursal,'xxx')
                           ,substr(mo.banco,1,3)
                           ,icd.unidad
                           ,mo.plazo_facial
                           ,mo.bursatilidad --Codigo bursatilidad
                           ,mo.sector_propio
                           ,mo.sector_garantizado
                           ,mo.clase_negocio_propia       -- Aqui va la propia
                           ,mo.clase_negocio_garantizada  -- Aqui va la garantizada.
                           ,mo.origen_titulo_propio
                           ,mo.origen_titulo_garantizado
                           ,t_cta_clase_inversion
                           ,t_cta_clase_inversion_a
                           ,t_cta_clase_inversion_b
                           ,t_cta_tipo_inversion
                           ,t_cta_tipo_inversion_a
                           ,t_cta_tipo_inversion_b
                           ,t_cta_emisor
                           ,t_cta_emisor_a
                           ,t_cta_emisor_b
                           ,t_cta_plazo
                           ,t_cta_plazo_a
                           ,t_cta_plazo_b
                           ,t_cta_especie
                           ,t_cta_especie_a
                           ,t_cta_especie_b
                           ,t_cta_sucursal
                           ,t_cta_cod_banco
                           ,t_cta_plazo_aut
                           ,t_cta_bur -- Cuenta asociada a Burs
                           ,t_cta_bur_a -- Cuenta A asociada a Burs
                           ,t_cta_bur_b -- Cuenta A asociada a Burs
                           ,t_cta_sector
                           ,t_cta_sector_a
                           ,t_cta_sector_b
                           ,t_cta_origen_garantia
                           ,t_cta_origen_garantia_a
                           ,t_cta_origen_garantia_b
                           ,t_pla_clase_inv                     -- caso 04032509
                           ,t_pla_tipo_inv                      -- caso 04032509
                           ,t_cta_plazo_aut_a                   -- caso 05122709
                           ,t_cta_moneda                        --08102911. 20090520
                           ,t_cta_moneda_a                      --08102911. 20090520
                           ,t_cta_moneda_b                      --08102911. 20090520
                           );
            end loop;
          end if;
          ----
          t_valor_base := NULL;

          PBD_LOG('con','1 titulo: '||mo.titulo||' concepto: '||t_concepto);

          IF INSTR(w_cuenta,'#N' ) = 0 THEN t_valor_base := fn_busque_valor(mo.operacion,cc.tipo_concepto,cc.concepto,cc.tipo_valor); END IF;
          -- Si existe asignelo, sino calc¿lelo.
          IF t_valor_base IS NOT NULL THEN
            pr_mensaje('Se resuelve a trav¿s de valor calculado previamente');
            w_valor := t_valor_base;
            PBD_LOG('con','2 titulo: '||mo.titulo||' concepto: '||t_concepto);
          ELSE
          PBD_LOG('con','3 titulo: '||mo.titulo||' concepto: '||t_concepto);
            t_concepto  := cc.concepto;  -- ORC 20071027
            t_tmp_valor := 0;            -- ORC 20071027
            w_valor     := 0;            -- ORC 20071027
            t_cuantos   := 1;            -- ORC 20071027
            t_pos_concepto := 1;            -- 08102911
            WHILE t_cuantos <= 3 LOOP       -- 08102911. Cambiar <= 2 por <= 3
            PBD_LOG('con','4 titulo: '||mo.titulo||' concepto: '||t_concepto);
              t_concepto := SUBSTR(cc.concepto,t_pos_concepto,2);  --08102911 Cambiar t_cuantos*t_cuantos por t_pos_concepto
              IF cc.tipo_concepto = 'F' AND t_concepto IS NOT NULL THEN  -- Concepto Financiero  -- ORC 20071027
                IF mo.calculo = 'F' THEN      -- Calculo de operacion es por formula
                  PBD_LOG('con','5 titulo: '||mo.titulo||' concepto: '||t_concepto);
                  pr_mensaje('Se resuelve '||t_concepto||' a trav¿s de fbd_valor_movto.');       -- ORC 20071027
                  t_error := Fbd_Valor_Movto(mo.clase,t_concepto,cc.tipo_valor,mo.titulo,mo.operacion,t_tmp_valor,pfm_fecha);  -- ORC 20071027
                  IF t_error <> 0 THEN RETURN(t_error); END IF;
                ELSE
                  PBD_LOG('con','6 titulo: '||mo.titulo||' concepto: '||t_concepto);
                  IF cc.tipo_valor = 'U' THEN  -- C¿lculo directo para unidades
                    PBD_LOG('con','7 titulo: '||mo.titulo||' concepto: '||t_concepto);
                    pr_mensaje('Se resuelve directo de ope_valor en moneda');
                    t_tmp_valor := mo.valor;                                                     -- ORC 20071027
                  ELSE                         -- C¿lculo directo para pesos
                    PBD_LOG('con','8 titulo: '||mo.titulo||' concepto: '||t_concepto);
                    pr_mensaje('Se resuelve directo de ope_valor en pesos');
                    t_tmp_valor := mo.valor * mo.precio * mo.cnv_precio;                         -- ORC 20071027
                  END IF;
                END IF;
              ELSIF cc.tipo_concepto = 'T' AND t_concepto IS NOT NULL THEN  -- Concepto Tributario   -- ORC 20071027
                PBD_LOG('con','9 titulo: '||mo.titulo||' concepto: '||t_concepto);
                pr_mensaje('Se resuelve a trav¿s de fbd_vr_concepto_contable');
                t_tmp_valor := Fbd_Vr_Concepto_Contable(mo.operacion   ,t_concepto,'T',mo.valor,t_base,'C',cc.tipo_valor);  -- ORC 20071027
              END IF;
              PBD_LOG('con','10');
              IF    t_cuantos = 1 THEN                   w_valor := t_tmp_valor;                                            -- ORC 20071027
              --08102911 Cambiar ELSIF t_cuantos = 2 THEN                                                                   -- ORC 20071027
              --por
              else
                --08102911. Cambiar SUBSTR(cc.concepto,3,1) por SUBSTR(cc.concepto,(t_pos_concepto-1),1)
                IF    SUBSTR(cc.concepto,(t_pos_concepto-1),1) = '+' THEN w_valor := w_valor + t_tmp_valor;                                  -- ORC 20071027
                ELSIF SUBSTR(cc.concepto,(t_pos_concepto-1),1) = '-' THEN w_valor := w_valor - t_tmp_valor;                                  -- ORC 20071027
                ELSIF SUBSTR(cc.concepto,(t_pos_concepto-1),1) = '*' THEN w_valor := w_valor * t_tmp_valor;                                  -- ORC 20071027
                END IF;                                                                                                     -- ORC 20071027
              END IF;
                                                                                                                     -- ORC 20071027
              pr_mensaje(' Concepto ---- '||t_concepto||' retorna '||t_tmp_valor);
              pr_mensaje(' Acumulado --- '||cc.concepto||' retorna '||w_valor);
              t_cuantos := t_cuantos+1;                                                                                     -- ORC 20071027
              --08102911. Incrementar la posicion del concepto
              t_pos_concepto := t_pos_concepto + t_constante;
            END LOOP;
                                                                                                     -- ORC 20071027
            pr_guarde_valor(mo.operacion,cc.tipo_concepto,cc.concepto,cc.tipo_valor,w_valor);
          END IF;
          pr_mensaje('Valor base del concepto '||cc.concepto||' '||LTRIM(TO_CHAR(w_valor,'999,999,999,999,999,999.9999')));
          IF cc.signo = '-' THEN w_valor := -w_valor; END IF;
          w_valor := (w_valor * cc.porcentaje) / 100;
          w_suc_origen  := REPLACE(REPLACE(cc.c_suc_origen,'#O',mo.o_suc_origen) ,'#D',mo.o_suc_origen);
          w_suc_destino := REPLACE(REPLACE(cc.c_suc_destino,'#O',mo.o_suc_origen),'#D',mo.o_suc_destino);

          pr_mensaje('Valor Real del concepto '||cc.concepto||' '||LTRIM(TO_CHAR(w_valor,'999,999,999,999,999,999.9999')));

          IF NVL(w_valor,0) > 0.001 AND (w_valor > NVL(cc.monto_base,0)) THEN
            w_nit           := NULL;
            w_naturaleza    := cc.naturaleza;
            w_cuenta        := cc.cuenta;
            w_cuenta_base   := cc.cuenta;        -- 08042916
            w_empresa       := pfm_empresa;
            w_portafolio    := pfm_portafolio;
            w_mercado       := mo.mercado;
            w_clase         := mo.clase;
            w_descripcion   := mo.descripcion;
            -- Para portafolio de emision, se coge como nit el
            -- 1er beneficiario activo del titulo. Si todavia no esta conocido
            -- (999999999999), se deja el nit del emisor
            IF cc.tipo_nit <> 'N' THEN
              IF mo.activo_pasivo != 'E' OR mo.beneficiario = 999999999999 THEN
                IF cc.tipo_nit = 'E' THEN
                   w_nit := mo.nit;          -- Nit del emisor
                ELSE
                   -- Si es un SWAP y no tiene nit del tercero dejarle el mismo de la compra
                   IF mo.c_swap = 'S' AND mo.nit_tercero IS NULL THEN
                      SELECT ope_per_ident
                        INTO w_nit
                        FROM PFOPERACIONES_V2
                       WHERE ope_tit_consec     = mo.titulo
                         AND ope_top_codigo||'' = '1'
                      ;
                   ELSE
                      w_nit := mo.nit_tercero;  -- Nit del tercero
                      if w_nit is null and mo.nit_tercero_garantizado is not null then w_nit := mo.nit_tercero_garantizado; end if; -- ORC 20080826
                   END IF;
                END IF;
              ELSE
                IF cc.tipo_nit = 'E' THEN
                  IF mo.tipo_operacion = 'CB' THEN -- Si el tipo operacion es "Cancel.Beneficiario" se toma el nit del anterior beneficiario
                    IF Fbd_Cliente = 'o16' THEN                            -- JCD 20050621 Se deja el mismo C¿digo generado por Bancoldex
                        BEGIN
                          SELECT bxt_per_ident                              --GPQ 2004/04/29
                            INTO t_nit
                            FROM PFBENEFICIARIOS_TITULO_V2
                           WHERE bxt_consec = (SELECT MIN(bxt_consec)
                                                 FROM PFBENEFICIARIOS_TITULO_V2
                                                WHERE bxt_f_inicial <= pfm_fecha
                                                  AND bxt_f_final   >= pfm_fecha
                                                  AND bxt_tit_consec = mo.titulo
                                              )
                          ;
                           EXCEPTION  WHEN OTHERS THEN t_nit := mo.beneficiario;
                        END;
                    ELSE
                        BEGIN
                          SELECT bxt_per_ident
                            INTO t_nit
                            FROM PFBENEFICIARIOS_TITULO_V2
                           WHERE bxt_consec = (SELECT MAX(bxt_consec)
                                                 FROM PFBENEFICIARIOS_TITULO_V2
                                                WHERE bxt_estado||'' = 'iIN'
                                                  AND bxt_tit_consec = mo.titulo
                                              )
                          ;
                          EXCEPTION  WHEN OTHERS THEN t_nit := mo.beneficiario;
                        END;
                    END IF;
                    w_nit:= t_nit;
                  ELSE
                     IF Fbd_Cliente = 'o16' THEN                             -- JCD 20050620
                        IF mo.modalidad = 'A' THEN                           -- GPQ 2004/04/29
                           IF mo.tipo_operacion = 'A' THEN
                              BEGIN
                                SELECT bxt_per_ident
                                  INTO t_nit
                                  FROM PFBENEFICIARIOS_TITULO_V2
                                 WHERE bxt_consec = (SELECT MIN(bxt_consec)
                                                       FROM PFBENEFICIARIOS_TITULO_V2
                                                      WHERE bxt_f_inicial <= pfm_fecha
                                                        AND NVL(bxt_f_final,pfm_fecha+1) >= pfm_fecha
                                                        AND bxt_tit_consec = mo.titulo
                                                    )
                                ;
                                EXCEPTION WHEN OTHERS THEN t_nit := mo.beneficiario;
                              END;
                              w_nit := t_nit;
                           ELSE
                              w_nit := fn_benef_hist(mo.titulo, pfm_fecha);
                           END IF;
                        ELSE
                              w_nit := fn_benef_hist(mo.titulo, pfm_fecha);
                        END IF;
                     ELSE                                                                        -- JCD 20050620
                        w_nit := mo.beneficiario; -- Para emision, E es el nit del beneficiario. -- JCD 20050620
                     END IF;                                                                     -- JCD 0
                   END IF;
                ELSE   w_nit := mo.nit_tercero;  -- Nit del intermediario.
                END IF;
              END IF;
            END IF;

           -- Inicio 11072508
            if cc.concepto = 'PA' then
              t_cta_riesgo := t_cta_riesgo_ayer;
              t_cta_riesgo_a := t_cta_riesgo_a_ayer;
            end if;
           -- Fin 11072508

            t_error := fn_cuenta(w_cuenta
                                ,t_cta_clase_inversion ,t_cta_clase_inversion_a   ,t_cta_clase_inversion_b
                                ,t_cta_tipo_inversion  ,t_cta_tipo_inversion_a    ,t_cta_tipo_inversion_b
                                ,t_cta_emisor          ,t_cta_emisor_a            ,t_cta_emisor_b
                                ,t_cta_plazo           ,t_cta_plazo_a             ,t_cta_plazo_b
                                ,t_cta_especie         ,t_cta_especie_a           ,t_cta_especie_b
                                ,t_cta_sucursal
                                ,t_cta_cod_banco
                                ,t_cta_plazo_aut
                                ,t_cta_bur             ,t_cta_bur_a               ,t_cta_bur_b
                                ,t_cta_riesgo          ,t_cta_riesgo_a
                                ,t_cta_sector          ,t_cta_sector_a            ,t_cta_sector_b
                                ,t_cta_origen_garantia ,t_cta_origen_garantia_a   ,t_cta_origen_garantia_b
                                ,t_cta_plazo_aut_a
                                ,t_cta_moneda          ,t_cta_moneda_a            ,t_cta_moneda_b          --08102911. 20090520
                                );

            IF t_error <> 0 THEN  RETURN(t_error); END IF;

            t_error := fn_descripcion(mo.especie ,mo.tipo_operacion ,mo.unidad     ,w_valor            ,mo.precio  ,mo.tasa_nominal
                                     ,mo.clase   ,mo.mercado        ,w_descripcion ,mo.fecha_operacion ,mo.empresa ,mo.activo_pasivo
                                     );

            IF t_error <> 0 THEN  RETURN(t_error); END IF;
            -- Cantidad de decimales a redondear
            -- Caso 04052718
            t_moneda_decimales := NULL;
            IF mo.clase = 'ZDE' THEN -- Divisas
              IF mo.tipo_operacion IN ('1','2') THEN -- Operacion Origen
                IF cc.tipo_valor = '$' THEN
                  t_moneda_decimales := Fbd_Emp_Unidad(mo.empresa);
                ELSIF cc.tipo_valor = 'L' THEN
                  t_moneda_decimales := mo.unidad; -- Local
                ELSIF cc.tipo_valor = 'U' THEN
                  t_moneda_decimales := mo.foranea;
                END IF;
              ELSE  -- es derivada
                t_moneda_decimales := mo.moneda_operacion;
              END IF;
            ELSE -- los demas portafolios
              IF cc.tipo_valor = '$' THEN
                t_moneda_decimales := Fbd_Emp_Unidad(mo.empresa);
              ELSIF cc.tipo_valor = 'U' THEN
                t_moneda_decimales := mo.unidad;
              END IF;
            END IF;
            IF NVL(t_moneda_decimales,mo.unidad) <> mo.unidad THEN
              -- determine nuevamente los decimales para redondeo
              glo_round_ant := glo_round;
              SELECT NVL(cnv_decimales,2)
                INTO glo_round
                FROM PFCONVERSIONES_V2
               WHERE cnv_codigo = t_moneda_decimales;
            END IF;

            w_valor := ROUND(w_valor,glo_round);
            -- para que dentro del for continue con el valor que traia
            IF glo_round_ant IS NOT NULL THEN
              glo_round := glo_round_ant;
              glo_round_ant := NULL;
            END IF;

            -- Fin Caso 04052718
            IF INSTR(w_cuenta,'#N' ) > 0 OR
               INSTR(w_cuenta,'#NA') > 0 OR
               INSTR(w_cuenta,'#NB') > 0 THEN
              -- El valor a contabilizar en cada detalle garantia, es el valor proporcionado entre Valor original a contabilizar
              -- el valor de compra de la inversion vista y el valor de la garantia:
              -- Valor = (w_valor*deg_valor)/(sum(deg_valor)

              -- Obtencion valor total garantias:
              t_total_garantias := 0;
              FOR icd IN c_det_garantia(mo.titulo) LOOP
                pr_mensaje('Garantia titulo '||icd.titulo||'   desde '||icd.f_desde||'   Hasta '||icd.f_hasta||'   Valor garantia '||TO_CHAR(icd.valor,'999,999,999,999.99'));
                IF pfm_fecha = icd.f_desde OR pfm_fecha = icd.f_hasta OR (pfm_fecha>=icd.f_desde AND pfm_fecha<icd.f_hasta) THEN
                  t_total_garantias := t_total_garantias + icd.valor;
                END IF;
              END LOOP;

              IF t_total_garantias > 0 THEN
                pr_mensaje('Cuenta a contabilizar valor particionado en cada detalle garantias');
                FOR icd IN c_det_garantia(mo.titulo) LOOP
                  IF pfm_fecha = icd.f_desde OR pfm_fecha = icd.f_hasta OR (pfm_fecha>=icd.f_desde AND pfm_fecha<icd.f_hasta) THEN
                    -- Si los conceptos dependen para su obtencion de informaci¿n tanto de la inversi¿n vista como d ela garant¿a,
                    -- se debe recalcular el concepto para cada garantia, de lo contario se puede proporicionalizar el valor calculado
                    -- del concepto por cada garantia.
                    IF cc.concepto IN ('R1','R2') THEN
                      IF    cc.concepto = 'R1' THEN
                            SELECT tit_f_cierre INTO t_fecha FROM PFTITULOS_V2 WHERE tit_consec = mo.titulo;
                            Pbd_Tirvp_Reportos(mo.titulo,t_fecha,'V','R1',t_tm,w_valor_detalle,icd.titulo);
                      ELSIF cc.concepto = 'R2' THEN
                            SELECT tit_f_cierre_ant INTO t_fecha FROM PFTITULOS_V2 WHERE tit_consec = mo.titulo;
                            Pbd_Tirvp_Reportos(mo.titulo,t_fecha,'V','R2',t_tm,w_valor_detalle,icd.titulo);
                      END IF;
                    ELSE
                      w_valor_detalle := (w_valor*icd.valor)/t_total_garantias;
                    END IF;
                    -- Resolver metacaracteres #N
                    w_cuenta_detalle := w_cuenta;
                    FOR icu IN c_cuentas_especie_gtia(mo.tipo_movto
                                                     ,mo.tipo_operacion
                                                     ,icd.clase
                                                     ,icd.tipo
                                                     ,icd.emisor
                                                     ,mo.clase_negocio_propia
                                                     ,mo.clase_negocio_garantizada
                                                     )
                    LOOP
                      w_cuenta_detalle := REPLACE(w_cuenta_detalle,'#NB',icu.ceg_cuenta_b);
                      w_cuenta_detalle := REPLACE(w_cuenta_detalle,'#NA',icu.ceg_cuenta_a);
                      w_cuenta_detalle := REPLACE(w_cuenta_detalle,'#N' ,icu.ceg_cuenta);
                      EXIT;
                    END LOOP;

                    pr_mensaje('Valor a insertar en pfmovtos_contables_v2 '||LTRIM(TO_CHAR(w_valor_detalle,'999,999,999,999,999,999.9999'))||'  en la cuenta '||w_cuenta_detalle);

                    SELECT pfseq_mco.NEXTVAL INTO consecutivo FROM sys.dual;
                    INSERT INTO PFMOVTOS_CONTABLES_V2
                      (mco_consec        ,mco_emp_codigo    ,mco_por_codigo   ,mco_top_codigo    ,mco_cto_modalidad ,mco_cto_mercado
                      ,mco_clase         ,mco_clasificacion ,mco_tit_consec   ,mco_nit           ,mco_fecha         ,mco_cuenta
                      ,mco_naturaleza    ,mco_valor         ,mco_tipo_valor   ,mco_ope_fecha     ,mco_ope_f_cobro   ,mco_descripcion
                      ,mco_tipo_movto    ,mco_fuente        ,mco_operacion    ,mco_comprobante   ,mco_procedencia   ,mco_suc_origen
                      ,mco_suc_destino   ,mco_ope_consec    ,mco_agrupamiento ,mco_auxiliar      ,mco_documento     ,mco_referencia
                      ,mco_concepto      ,mco_signo         ,mco_porcentaje   ,mco_tipo_concepto ,mco_rco_codigo    ,mco_ctro_costo
                      ,mco_auxiliar1     ,mco_precio        ,mco_cnv_precio   ,mco_cuenta_base
                      )
                    VALUES
                      (consecutivo       ,w_empresa         ,w_portafolio     ,mo.tipo_operacion ,w_modalidad       ,w_mercado
                      ,w_clase           ,w_clasificacion   ,mo.titulo        ,w_nit             ,pfm_fecha         ,w_cuenta_detalle
                      ,w_naturaleza      ,w_valor_detalle   ,cc.tipo_valor    ,mo.fecha_operacion,mo.fecha_cobro    ,SUBSTR(w_descripcion,1,70)
                      ,mo.tipo_movto     ,mo.fuente         ,cc.operacion     ,cc.comprobante    ,cc.procedencia    ,w_suc_origen
                      ,w_suc_destino     ,mo.operacion      ,cc.agrupamiento  ,cc.auxiliar       ,cc.documento      ,cc.referencia
                      ,cc.concepto       ,cc.signo          ,cc.porcentaje    ,cc.tipo_concepto  ,mo.ref_contable   ,mo.ctro_costo
                      ,cc.auxiliar1      ,mo.precio         ,mo.cnv_precio    ,w_cuenta_base
                      );
                  END IF;
                END LOOP;
              END IF;
            ELSE
              pr_mensaje('Valor a insertar en pfmovtos_contables_v2 '||LTRIM(TO_CHAR(w_valor,'999,999,999,999,999,999.9999'))||'  en la cuenta '||w_cuenta);
              SELECT pfseq_mco.NEXTVAL INTO consecutivo FROM sys.dual;
              INSERT INTO PFMOVTOS_CONTABLES_V2
                (mco_consec        ,mco_emp_codigo    ,mco_por_codigo   ,mco_top_codigo    ,mco_cto_modalidad ,mco_cto_mercado
                ,mco_clase         ,mco_clasificacion ,mco_tit_consec   ,mco_nit           ,mco_fecha         ,mco_cuenta
                ,mco_naturaleza    ,mco_valor         ,mco_tipo_valor   ,mco_ope_fecha     ,mco_ope_f_cobro   ,mco_descripcion
                ,mco_tipo_movto    ,mco_fuente        ,mco_operacion    ,mco_comprobante   ,mco_procedencia   ,mco_suc_origen
                ,mco_suc_destino   ,mco_ope_consec    ,mco_agrupamiento ,mco_auxiliar      ,mco_documento     ,mco_referencia
                ,mco_concepto      ,mco_signo         ,mco_porcentaje   ,mco_tipo_concepto ,mco_rco_codigo    ,mco_ctro_costo
                ,mco_auxiliar1     ,mco_precio        ,mco_cnv_precio   ,mco_cuenta_base
                )
              VALUES
                (consecutivo       ,w_empresa         ,w_portafolio     ,mo.tipo_operacion ,w_modalidad       ,w_mercado
                ,w_clase           ,w_clasificacion   ,mo.titulo        ,w_nit             ,pfm_fecha         ,w_cuenta
                ,w_naturaleza      ,w_valor           ,cc.tipo_valor    ,mo.fecha_operacion,mo.fecha_cobro    ,SUBSTR(w_descripcion,1,70)
                ,mo.tipo_movto     ,mo.fuente         ,cc.operacion     ,cc.comprobante    ,cc.procedencia    ,w_suc_origen
                ,w_suc_destino     ,mo.operacion      ,cc.agrupamiento  ,cc.auxiliar       ,cc.documento      ,cc.referencia
                ,cc.concepto       ,cc.signo          ,cc.porcentaje    ,cc.tipo_concepto  ,mo.ref_contable   ,mo.ctro_costo
                ,cc.auxiliar1      ,mo.precio         ,mo.cnv_precio    ,w_cuenta_base
                );
            END IF;
            IF w_naturaleza = 'D' THEN t_debitos  := t_debitos  + w_valor; END IF;
            IF w_naturaleza = 'C' THEN t_creditos := t_creditos + w_valor; END IF;
          END IF;
        END LOOP;     -- De cuentas
        -- Ajustar descuadres por redondeos.
        IF t_debitos <> t_creditos AND consecutivo IS NOT NULL
          AND ABS(t_debitos-t_creditos) < 10
          AND (t_debitos > 10 OR t_debitos > 10) THEN
          -- Si debitos > creditos  Hay que sumar a un credito o restar a un debito
          -- Si debitos < creditos  Hay que restar a un credito o sumar a un debito
          -- El proceso se hace sobre la ultima fila insertada por tipo de operacion.
          UPDATE PFMOVTOS_CONTABLES_V2
             SET mco_valor = mco_valor + ABS(t_debitos - t_creditos) *
                                         DECODE(SIGN(t_debitos-t_creditos)
                                               , 1,DECODE(w_naturaleza,'D',-1, 1)
                                               ,-1,DECODE(w_naturaleza,'D', 1,-1)
                                               )
           WHERE mco_consec = consecutivo
          ;
        END IF;
      END IF;
      IF pfm_elimina = 'S' THEN
         DELETE FROM PFHIST_OPERACIONES_V2 WHERE ope_consec = mo.operacion;
         DELETE FROM PFOPERACIONES_V2      WHERE ope_consec = mo.operacion;
      ELSE
        UPDATE PFOPERACIONES_V2
           SET ope_estado   = 'OCA'
              ,ope_f_contab = pfm_fecha
         WHERE ope_consec = mo.operacion;
      END IF;
      pr_mensaje('Operacion procesada.');
    end;
  END LOOP;       -- De operaciones
  --
  IF Fbd_Apl_Instalada('TSFLUJO') = 'S' AND pfm_elimina = 'N' THEN
    t_error := Fbd_Movto_Flujo_Caja(pfm_empresa,pfm_portafolio,pfm_fecha);
  END IF;
  RETURN(0);
EXCEPTION
  WHEN OTHERS THEN
    IF SQLCODE IN (-20102,-20103) THEN
    pbd_log('FDE','Error_movto1.1: '||DBMS_UTILITY.FORMAT_call_stack);
    pbd_log('FDE','Error_movto1.2: '||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
          ROLLBACK TO sp_movto_contable;
          RAISE_APPLICATION_ERROR(SQLCODE,SQLERRM);
    ELSE
    pbd_log('FDE','Error_movto2.1: '||DBMS_UTILITY.FORMAT_call_stack);
    pbd_log('FDE','Error_movto2.2: '||DBMS_UTILITY.FORMAT_ERROR_BACKTRACE);
      RETURN(Fbd_Titulo_Error(glo_titulo,SQLCODE));
    END IF;
END fbd_movto_contable;








/
-------------------------------------------------------------------------------
Show err
