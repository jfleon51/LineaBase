---------------------------------------------------------------------------------
-- PROGRAMA  : PFICO310.sql
-- OBJETIVO  : Generar el archivo plano de Información Portafolios de Inversión
--             Valoración formato 351
-- PARAMETROS: Empresa o Consolidador
-------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------------------------------------------------------------------
-- Modificaciones:
-- CASO     CONSULTOR    FECHA     DESCRIPCIÓN CAMBIO
-- 12101701    JMF     2012/11/09  En el registro tipo 4 para el formato 351, se incluye el tipo de entidad y código del proveerdor de precios.
-- 13030541    JMF     2013/03/06  Se cambia el argumento con el q se llama a pkg_segmentos.fn_proveedor_segmento, pue sno se tiene en contexto la empresa
---------------------------------------------------------------------------------------------------------------------------------------------------------

  define _tamrep = 85
  define _codrep='PFICO310'

  -- Eliminar objetos locales
  drop table     tmp_reg_tipo1_310;
  drop table     pftmp_log;
  drop sequence  pfseq_tmp_310;
  drop sequence  pfseq_formato_320;
  drop function  fn_es_numerico_310;
  drop procedure pr_complemento_emisor_310_SAF;
  drop procedure pr_complemento_emisor_310_SAE;
  drop procedure pr_datos_tipo3_310;
  drop procedure pr_datos_tipo4_310;
  drop procedure pr_datos_tipo5_310;
  drop procedure pr_datos_formato_320;

  @@id1pflibpla.sql

  create sequence  pfseq_tmp_310
   increment by     1
   start with       1
   minvalue         1
   maxvalue         99999999999999999999
   nocycle
   cache            2
   order
  ;

  create sequence  pfseq_formato_320
   increment by     1
   start with       1
   minvalue         1
   maxvalue         99999999999999999999
   nocycle
   cache            2
   order
  ;

  set linesize 100
  set trims     on

  create table tmp_reg_tipo1_310
       (tmp1_secuencia      number
       ,tmp1_tipo_reg       number
       ,tmp1_tipo_ent       varchar2(3)
       ,tmp1_cod_entid      varchar2(6)
       ,tmp1_fecha          date
       ,tmp1_tot_reg        number
       ,tmp1_clave          varchar2(11)
       ,tmp1_area_info      number
       ,tmp1_tipo_inf       number
       ,tmp3_tipo_eval      number
       ,tmp3_tipo_fide      number
       ,tmp3_cod_fide       number
       ,tmp3_tipo_ent       number
       ,tmp3_cod_ent        number
       ,tmp3_tipo_ent_cons  number
       ,tmp3_cod_ent_cons   number
       ,tmp3_tipo_fide_cons number
       ,tmp3_cod_fide_cons  number
       ,tmp4_tipo_ident     varchar2(1)
       ,tmp4_num_ident      varchar2(15)
       ,tmp4_dig_chequeo    varchar2(1)
       ,tmp4_nombre         varchar2(50)
       ,tmp4_cod_ciiu       number
       ,tmp4_cod_natu       number
       ,tmp4_cam_var1       number
       ,tmp4_cam_var2       number
       ,tmp4_cam_var3       number
       ,tmp4_cod_repor      number
       ,tmp5_cod_formato    number
       ,tmp5_cod_columna    number
       ,tmp5_cod_und_cap    number
       ,tmp5_cod_subcta     number
       ,tmp5_signo          varchar2(1)
       ,tmp5_valor          varchar2(50)
       ,tmp5_fideicomiso    varchar2(100)
       ,tmp6_tot_reg        number
       )
       ;


---------------------------------------------------------------------------------------------
  -- Esta funcion retorna 1 si la cadena es numèrica de lo contrario retorna 0
---------------------------------------------------------------------------------------------
create or replace
function fn_es_numerico_310(t_cadena varchar2) return number is
  t_cad number;
begin
  select to_number(t_cadena)
    into t_cad
    from dual
    ;
    return(1);
  exception when others then
    return(0);
end;
/

--obtiene la informaciòn parametrizada bajo la entidad SAF para una empresa
-- devuelve los valores de:
--                         tipo de evaluaciòn
--                         tipo de fidecomiso
--                         codigo del fidecomiso
--                         tipo de entidad vigilada
--                         codigo de la entidad vigilada
create or replace
procedure pr_complemento_emisor_310_SAF(p_fide           varchar2
                                   ,p_tip_eval       in out  number
                                   ,p_tip_fid        in out  number
                                   ,p_cod_fid        in out  number
                                   ,p_tip_entid      in out  number
                                   ,p_cod_entid      in out  number
                                   ) is
t_long_cad           number;
t_posicion           number;
t_cadena             varchar2(25);
begin
  t_cadena := p_fide ;
  --Se busca el tipo de evaluación
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_tip_eval     := substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el tipo de fidecomiso
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_tip_fid  := substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el código del fidecomiso
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_cod_fid  := substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el tipo de entidad vigilada
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_tip_entid:= substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el codigo de la entidad
  p_cod_entid:= t_cadena ;
end;
/

create or replace
procedure pr_complemento_emisor_310_SAE(p_fide           varchar2
                                   ,p_tip_entid      in out  number
                                   ,p_cod_entid      in out  number
                                   ,p_tip_fid        in out  number
                                   ,p_cod_fid        in out  number
                                   ) is
t_long_cad           number;
t_posicion           number;
t_cadena             varchar2(25);
begin
  t_cadena := p_fide ;
  --Se busca el tipo de entidad vigilada
  t_long_cad  := length(t_cadena);
  t_posicion  := instr(t_cadena,'-');
  p_tip_entid := substr(t_cadena,1,t_posicion-1);
  t_cadena    := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el codigo de la entidad
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_cod_entid:= substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el tipo de fidecomiso
  t_long_cad := length(t_cadena);
  t_posicion := instr(t_cadena,'-');
  p_tip_fid  := substr(t_cadena,1,t_posicion-1);
  t_cadena   := substr(t_cadena,t_posicion+1,t_long_cad);

  --Se busca el código del fidecomiso
  p_cod_fid  := t_cadena ;
end;
/

---------------------------------------------------------------------------------------------
-- Inserta tipo de registro 3
---------------------------------------------------------------------------------------------
create or replace
procedure pr_datos_tipo3_310(p_tip_eval       number
                            ,p_tip_fid        number
                            ,p_cod_fid        number
                            ,p_tip_entid      number
                            ,p_cod_entid      number
                            ,p_tip_entid_cons number
                            ,p_cod_entid_cons number
                            ,p_tip_fid_cons   number
                            ,p_cod_fid_cons   number
                            ) is
t_secuencia2 number := 0;
begin

  select pfseq_tmp_310.nextval
    into t_secuencia2
    from dual;

  insert into tmp_reg_tipo1_310(tmp1_secuencia
                               ,tmp1_tipo_reg
                               ,tmp3_tipo_eval
                               ,tmp3_tipo_fide
                               ,tmp3_cod_fide
                               ,tmp3_tipo_ent
                               ,tmp3_cod_ent
                               ,tmp3_tipo_ent_cons
                               ,tmp3_cod_ent_cons
                               ,tmp3_tipo_fide_cons
                               ,tmp3_cod_fide_cons
                               )
                         values(t_secuencia2                        -- secuencia
                               ,3                                   -- tipo registro
                               ,nvl(p_tip_eval,0)                   -- eso o no fidecomiso
                               ,nvl(p_tip_fid,0)                    -- tipo fidecomiso
                               ,nvl(p_cod_fid,0)                    -- codigo fidecomiso
                               ,nvl(p_tip_entid,0)                  -- tipo entidad vigilada
                               ,nvl(p_cod_entid,0)                  -- codigo entidad vigilada
                               ,nvl(p_tip_entid_cons,0)             -- tipo entidad vigilada consorcio
                               ,nvl(p_cod_entid_cons,0)             -- codigo entidad vigilada consorcio
                               ,nvl(p_tip_fid_cons,0)               -- tipo fidecomiso consorcio
                               ,nvl(p_cod_fid_cons,0)               -- codigo fidecomiso consorcio
                               )
                               ;
end;
/

---------------------------------------------------------------------------------------------
-- Inserta tipo de registro 4
---------------------------------------------------------------------------------------------
create or replace
procedure pr_datos_tipo4_310(p3_tip_eval       number
                            ,p3_tip_fid        number
                            ,p3_cod_fid        number
                            ,p3_tip_entid      number
                            ,p3_cod_enti       number
                            ,p3_tip_entid_cons number
                            ,p3_cod_entid_cons number
                            ,p3_tip_fid_cons   number
                            ,p3_cod_fid_cons   number
                            ,p4_tipo_emi       varchar2
                            ,p4_nit_emi        varchar2
                            ,p4_dig_chequeo    varchar2
                            ,p4_nombre         varchar2
                            ,p4_cod_emi        varchar2
                            ,p4_reporta        number
                            ,p4_tipo           varchar2
                            ,p4_tip_proveedor  number default null    --12101701
                            ,p4_cod_proveedor  number default null    --12101701
                            ) is
t_secuencia3 number := 0;
begin
  select pfseq_tmp_310.nextval
    into t_secuencia3
    from dual;

  insert into tmp_reg_tipo1_310(tmp1_secuencia
                               ,tmp1_tipo_reg
                               ,tmp3_tipo_eval
                               ,tmp3_tipo_fide
                               ,tmp3_cod_fide
                               ,tmp3_tipo_ent
                               ,tmp3_cod_ent
                               ,tmp3_tipo_ent_cons
                               ,tmp3_cod_ent_cons
                               ,tmp3_tipo_fide_cons
                               ,tmp3_cod_fide_cons
                               ,tmp4_tipo_ident
                               ,tmp4_num_ident
                               ,tmp4_dig_chequeo
                               ,tmp4_nombre
                               ,tmp4_cod_ciiu
                               ,tmp4_cod_natu
                               ,tmp4_cam_var1
                               ,tmp4_cam_var2
                               ,tmp4_cam_var3
                               ,tmp4_cod_repor
                               )
                     values(t_secuencia3                                           --secuencia
                               ,4                                                      -- tipo registro
                               ,p3_tip_eval                                            -- Tipo de evaluación
                               ,p3_tip_fid                                             -- Tipo de fideicomiso
                               ,p3_cod_fid                                             -- Código del fideicomiso
                               ,p3_tip_entid                                           -- Tipo de entidad vigilada originaria o fideicomitente
                               ,p3_cod_enti                                            -- Código de entidad vigilada originaria o fideicomitente
                               ,p3_tip_entid_cons                                      -- Tipo entidad, representante legal del consorcio
                               ,p3_cod_entid_cons                                      -- Código entidad, representante legal  del consorcio
                               ,p3_tip_fid_cons                                        -- Tipo fideicomiso del representante legal del consorcio
                               ,p3_cod_fid_cons                                        -- Código del fideicomiso del representante legal del consorcio
                               ,p4_tipo_emi                                            -- tipo de identificacion
                               ,nvl(p4_nit_emi,0)                                      -- numero identificacion
                               ,nvl(p4_dig_chequeo,0)                                  -- Dígito de chequeo
                               ,nvl(p4_nombre,0)                                       -- Nombre o razón social
                               ,decode(p4_tipo,'E',nvl(fbd_equivalencia_externa('EMISOR'
                                                                               ,'SAI'
                                                                               ,p4_cod_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                                  ,nvl(fbd_equivalencia_externa('TERCERO'
                                                                               ,'SAI'
                                                                               ,p4_nit_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                       )                                        -- codigo CIIU
                               ,decode(p4_tipo,'E',nvl(fbd_equivalencia_externa('EMISOR'
                                                                               ,'SAN'
                                                                               ,p4_cod_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                                  ,nvl(fbd_equivalencia_externa('TERCERO'
                                                                               ,'SAN'
                                                                               ,p4_nit_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                       )                                       -- Código naturaleza jurídica
                               ,decode(p4_tipo,'E',nvl(fbd_equivalencia_externa('EMISOR'
                                                                               ,'SAR'
                                                                               ,p4_cod_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                                  ,nvl(fbd_equivalencia_externa('TERCERO'
                                                                               ,'SAR'
                                                                               ,p4_nit_emi
                                                                               ,null,null,null,null)
                                                      ,0)
                                       )                                               -- campo variable1
                               ,nvl(p4_tip_proveedor,0)                                -- campo variable2    -- 12101701 Se incluye el valor del parametro y el nvl
                               ,nvl(p4_cod_proveedor,0)                                -- campo variable3    -- 12101701 Se incluye el valor del parametro y el nvl
                               ,p4_reporta                                             -- codigo reportado
                               )
                               ;
end;
/


---------------------------------------------------------------------------------------------
-- Inserta tipo de registro 5
---------------------------------------------------------------------------------------------
create or replace
procedure pr_datos_tipo5_310(p_formato    number
                          ,p_columna    number
                          ,p_unidad     number
                          ,p_subcta     number
                          ,p_valor      varchar2
                          ,p_fidei      varchar2
                          ) is
t_secuencia4 number := 0;
begin
  select pfseq_tmp_310.nextval
    into t_secuencia4
    from dual;
   insert into  tmp_reg_tipo1_310(tmp1_secuencia
                                 ,tmp1_tipo_reg
                                 ,tmp5_cod_formato
                                 ,tmp5_cod_columna
                                 ,tmp5_cod_und_cap
                                 ,tmp5_cod_subcta
                                 ,tmp5_signo
                                 ,tmp5_valor
                                 ,tmp5_fideicomiso
                                 )
                          values(t_secuencia4
                                ,5
                                ,p_formato
                                ,p_columna
                                ,p_unidad
                                ,p_subcta
                                ,decode(p_valor,'0','+'
                                                    ,decode(fn_es_numerico_310(p_valor),1,decode(sign(p_valor),1,'+','-')
                                                                   ,'+'))
                                ,nvl(p_valor,0)
                                ,p_fidei
                                )
                               ;
end;
/

------------------------------------------------------------------------------
-- Funcion que inserta el valor de una columna, como registro tipo 5 y
-- retorna 1 si pudo insertar, es decir si el valor de la columna no era nula
-------------------------------------------------------------------------------
create or replace
procedure pr_datos_formato_320 (p_columna      number
                               ,p_nom_tabla     varchar2
                               ,p_secuen        number
                               ,p_cod_formato   number
                               ,p_unidad_cap    number
                               ,p_subcta        number
                               ,p_fondo         varchar2
                               ) is

--- Definicion de variables
t_area_cursor        integer;
t_cargar_cursor      number;
t_query              varchar2(2000);
t_nom_col            varchar2(50);
t_secuencia2         number:=0;

--- Inicio de la funcion
begin
  --- Construye el Query dinamico
  t_area_cursor    := dbms_sql.open_cursor;
  t_query := 'select tmp_c' ||p_columna
             ||' from '||p_nom_tabla
             ||' where tmp_secuen ='|| p_secuen
                ;
  dbms_sql.parse(t_area_cursor,t_query,0);
  dbms_sql.define_column(t_area_cursor,1,t_nom_col,50);


  -- Ejecutar el cursor
  t_cargar_cursor := dbms_sql.execute(t_area_cursor);
  loop
    if dbms_sql.fetch_rows(t_area_cursor) > 0 then
      dbms_sql.column_value(t_area_cursor, 1,t_nom_col);
    else
      exit;
    end if;
  end loop;
  ---Cierra el cursor
  dbms_sql.close_cursor(t_area_cursor);

  --- Insercion del registro
  if ltrim(rtrim(t_nom_col)) is not null or t_nom_col = '}' then
    select pfseq_formato_320.nextval
      into t_secuencia2
      from dual;

        pr_datos_tipo5_310(p_cod_formato
                          ,p_columna
                          ,p_unidad_cap
                          ,p_subcta
                          ,t_nom_col
                          ,p_fondo
                          );

  end if;
end;
/


-------------------------------------------------------------------------------------------------
  --cursores con la información de los títulos ordena por emisor
-----------------------------------------------------------------------------------------------
  declare
  cursor c_empresas_porta is
      select view_emp_codigo   c_empresa
            ,view_por_codigo   c_port
            ,eme_emp_ent
        from pfview_portafolios_v2
            ,pfempresa_entidad_v2
       where view_emp_codigo = eme_emp_codigo
         and eme_ref_codigo  = 'SAF'
       order by 3,1
    ;

   cursor c_fondos310 is
     select distinct eme_emp_ent c_fondo
           ,eme_emp_codigo
       from pfview_portafolios_v2
           ,pfempresa_entidad_v2
      where view_emp_codigo = eme_emp_codigo
        and eme_ref_codigo  = 'SAF'
      order by 1
    ;

   cursor c_titulos(t_ent_codigo varchar2, t_empresa varchar2) is
      select tmp_secuen   ,tmp_cod_form , tmp_nom_form, tmp_tipo_form, tmp_unidad_cap
            ,tmp_renglon  , tmp_c1      , tmp_c2       , tmp_c3
            ,tmp_c4       , tmp_c5      , tmp_c6       , tmp_c7
            ,tmp_c8       , tmp_c9      , tmp_c10      , tmp_c11
            ,tmp_c12      , tmp_c13     , tmp_c14      , tmp_c15
            ,tmp_c16      , tmp_c17     , tmp_c18      , tmp_c19
            ,tmp_c20      , tmp_c21     , tmp_c22      , tmp_c23
            ,tmp_c24      , tmp_c25     , tmp_c26      , tmp_c27
            ,tmp_c28      , tmp_c29     , tmp_c30      , tmp_c31
            ,tmp_c32      , tmp_c33     , tmp_c34      , tmp_c35
            ,tmp_c36      , tmp_c37     , tmp_c38      , tmp_c39
            ,tmp_c40      , tmp_c41     , tmp_c42      , tmp_c43
            ,tmp_c44      , tmp_c45     , tmp_c46      , tmp_c47
            ,tmp_c48      , tmp_c49     , tmp_c50      , tmp_c51
            ,tmp_c52      , tmp_c53     , tmp_c54      , tmp_c55
            ,tmp_c56      , tmp_c57     , tmp_c58      , tmp_c59
            ,tmp_c60      , tmp_c61     , tmp_c62      , tmp_c63
            ,tmp_c64      , tmp_c65     , tmp_c66      , tmp_c67
            ,tmp_c68      , tmp_c69     , tmp_c70      , nvl(tmp_c71,'N') tmp_c71
            ,tmp_c72      , tmp_c73     , tmp_c74
            ,tmp_proveedor_pcio                                                       -- 12101701
        from pfempresa_entidad_v2
            ,pfdetalle_formato351_v2
       where eme_emp_ent     = t_ent_codigo
         and eme_emp_codigo  = t_empresa
         and eme_ref_codigo  = 'SAF'
         and tmp_tipo_form   = 'SECCION'
         and tmp_c74         = eme_emp_codigo
      union
      select tmp_secuen   ,tmp_cod_form , tmp_nom_form, tmp_tipo_form, tmp_unidad_cap
            ,tmp_renglon  , tmp_c1      , tmp_c2       , tmp_c3
            ,tmp_c4       , tmp_c5      , tmp_c6       , tmp_c7
            ,tmp_c8       , tmp_c9      , tmp_c10      , tmp_c11
            ,tmp_c12      , tmp_c13     , tmp_c14      , tmp_c15
            ,tmp_c16      , tmp_c17     , tmp_c18      , tmp_c19
            ,tmp_c20      , tmp_c21     , tmp_c22      , tmp_c23
            ,tmp_c24      , tmp_c25     , tmp_c26      , tmp_c27
            ,tmp_c28      , tmp_c29     , tmp_c30      , tmp_c31
            ,tmp_c32      , tmp_c33     , tmp_c34      , tmp_c35
            ,tmp_c36      , tmp_c37     , tmp_c38      , tmp_c39
            ,tmp_c40      , tmp_c41     , tmp_c42      , tmp_c43
            ,tmp_c44      , tmp_c45     , tmp_c46      , tmp_c47
            ,tmp_c48      , tmp_c49     , tmp_c50      , tmp_c51
            ,tmp_c52      , tmp_c53     , tmp_c54      , tmp_c55
            ,tmp_c56      , tmp_c57     , tmp_c58      , tmp_c59
            ,tmp_c60      , tmp_c61     , tmp_c62      , tmp_c63
            ,tmp_c64      , tmp_c65     , tmp_c66      , tmp_c67
            ,tmp_c68      , tmp_c69     , tmp_c70      , nvl(tmp_c71,'N') tmp_c71
            ,tmp_c72      , tmp_c73     , tmp_c74
            ,tmp_proveedor_pcio                                                       -- 12101701
        from pfdetalle_f351_externo_v2
            ,pfview_portafolios_v2
            ,pfempresa_entidad_v2
            ,pfreferencias_v2
       where tmp_c74         = view_emp_codigo
         and tmp_tipo_form   = 'SECCION'
         and eme_emp_ent     = t_ent_codigo
         and eme_ref_codigo  = 'SAF'
         and eme_emp_codigo  = tmp_c74
         and ref_codigo      = 'DFE'
         and ref_valor       = 'S'
       order by tmp_c70, tmp_unidad_cap
      ;

  cursor c_titulos397(t_ent_codigo varchar2) is
      select tmp_secuen   ,tmp_cod_form , tmp_nom_form, tmp_tipo_form, tmp_unidad_cap
                ,tmp_renglon  , tmp_c1      , tmp_c2       , tmp_c3
                ,tmp_c4       , tmp_c5      , tmp_c6       , tmp_c7
                ,tmp_c8       , tmp_c9      , tmp_c10      , tmp_c11
                ,tmp_c12      , tmp_c13     , tmp_c14      , tmp_c15
                ,tmp_c16      , tmp_c17     , tmp_c18      , tmp_c19
                ,tmp_c20      , tmp_c21     , tmp_c22      , tmp_c23
                ,tmp_c24      , tmp_c25     , tmp_c26      , tmp_c27
                ,tmp_c28      , tmp_c29     , tmp_c30      , tmp_c31
                ,tmp_c32      , tmp_c33     , tmp_c34      , tmp_c35
                ,tmp_c36      , tmp_c37
            from pfempresa_entidad_v2
                ,pfdetalle_formato397_v2
           where eme_emp_ent     = t_ent_codigo
             and eme_ref_codigo  = 'SAF'
             and tmp_tipo_form ='SECCION'
             and tmp_c74         = eme_emp_codigo
           order by tmp_c37
      ;

-----------------------------------------------------------------------------------------------
-- Variables
-----------------------------------------------------------------------------------------------
  t_columna            number;
  t_asegura            varchar2(1);
  t_subcta             number := 1 ;
  t_tot_emp            number;
  t_val_col            varchar2(50);
  t_col                number;
  t_tot_reg            number := 0 ;
  t_tipo_entidad       varchar2(3);
  t_cod_entidad        varchar2(6);
  t_clave              varchar2(11);
  t_tip_eval           number;
  t_tip_fid            number;
  t_cod_fid            number;
  t_tip_entid          number;
  t_cod_entid          number;
  t_eval_cons          number;
  t_tip_entid_cons     number;
  t_cod_entid_cons     number;
  t_tip_fid_cons       number;
  t_cod_fid_cons       number;
  t_long_cad           number;
  t_posicion           number;
  t_cadena             varchar2(25);
  t_emp                varchar2(3);
  t_area_cursor        integer;
  t_cargar_cursor      number;
  t_query              varchar2(2000);
  t_nom_col            varchar2(50);
  t_entra              number;
  t_valcol3            number;
  t_reg                number := 0;
  t_max_unidad         number := 0;
  t_borra              number := 1;
  t_tipo5              number := 0;
  t_tipo5_397          number := 0;
  t_max_renglon        number := 0;
  ant_fideicomiso      varchar2(100) := null;
  t_inf_consorcio      pfempresa_entidad_v2.eme_emp_ent%type;
  t_secuencia          number := 0;
  t_secuencia5         number := 0;
  T_FORMATO            VARCHAR2(50);
  T_BORRA_1            number := 1;
  T_NOMBRE             VARCHAR2(50);
  t_fon_ant            varchar2(100);
  t_fondo_ant          varchar2(100);
  t_tip_proveed        varchar2(1);               -- 12101701
  t_cod_proveed        varchar2(1);               -- 12101701
  t_codigo_prv         pfproveedores_v2.prv_codigo%type;  -- 12101701    
  t_emp_cod            pfempresas_v2.emp_codigo%type := null;    -- 13030541  
  
begin
  -- Se ejecuta el procedimiento por cada empresa y portafolio que conforma el consolidador
  -- esto hace que la información sea almacenada en la tabla pfdetalle_formato351_v2.
  for icm in c_empresas_porta loop
   --t_borra la prime vez va en uno para que en procedimiento se limpie la tabla pfdetalle_formato351_v2
   --solo debe ser borrada una vez
    T_FORMATO := FBD_EQUIVALENCIA_EXTERNA('PORTAFOLIO','SFR',ICM.C_EMPRESA,ICM.C_PORT,NULL,NULL,NULL);
    IF T_FORMATO = '351' THEN
      -- Inicio 13030541
      if t_emp_cod is null then
        t_emp_cod := icm.c_empresa;
      end if;
      -- Fin 13030541
      pbd_cargue_formato351(to_date('&3','yyyymmdd'),icm.c_empresa, icm.c_port,t_borra,'310');
      t_borra := 0;
    ELSIF T_FORMATO = '397' THEN
      pbd_cargue_formato397(to_date('&3','yyyymmdd'),icm.c_empresa, icm.c_port,t_borra_1);
      T_BORRA_1 := 0;
    END IF;
  end loop;

  ----------------------------------------------------------------------------------------
    --Registro tipo 1
  ----------------------------------------------------------------------------------------

  select distinct nvl(fbd_equivalencia_externa('EMPRESA'
                                      ,'SAT'
                                      ,view_emp_codigo
                                      ,null,null,null,null)
             ,'Er1')                                      -- Tipo de Entidad
         ,nvl(fbd_equivalencia_externa('EMPRESA'
                                      ,'SAC'
                                      ,view_emp_codigo
                                      ,null,null,null,null)
             ,'Er2')                                      -- Código de Entidad
         ,upper(nvl(fbd_equivalencia_externa('EMPRESA'
                                 ,'SAP'
                                 ,view_emp_codigo
                                 ,null,null,null,null)
             ,'Er3'))                                      -- Palabra Clave
  into t_tipo_entidad
      ,t_cod_entidad
      ,t_clave
  from pfview_portafolios_v2
  where rownum = 1
  ;

  select pfseq_tmp_310.nextval
    into t_secuencia
    from dual;

  insert into tmp_reg_tipo1_310(tmp1_secuencia
                             ,tmp1_tipo_reg
                             ,tmp1_tipo_ent
                             ,tmp1_cod_entid
                             ,tmp1_fecha
                             ,tmp1_clave
                             ,tmp1_area_info
                             ,tmp1_tipo_inf
                             )
                       values(t_secuencia                  -- Número secuencia: Para este caso es valor fijo 1
                             ,1                            -- Número secuencia: Para este caso es valor fijo 1
                             ,t_tipo_entidad               -- Tipo entidad
                             ,t_cod_entidad                -- Codigo entidad
                             ,to_date('&3','yyyymmdd')     -- Fecha: Fecha en que se solicita que sea  generado el informe
                             ,t_clave
                             ,4                            -- Área de información: es un valor fijo  4.
                             ,decode(fbd_cliente,'o32',8
                                                ,'o33',8
                                                ,0
                                    )                      -- Tipo informe, para este caso es 0 porque se esta generando formato 351
                             )
                             ;

  t_tot_emp := 0;

  for icf in c_fondos310  loop
    -- Reservar area de memoria para cargar cursor del registro tipo 5
    --------------------------------------------------------------------------------------
    --Registro tipo3
    --Solo crea un Registro Tipo3 si encuentra codigos de fondo diferentes
    --------------------------------------------------------------------------------------
    if nvl(t_fondo_ant,'0') <> icf.c_fondo then   --- Cambio de fondo y debe crearse un registro tipo 3
      pr_complemento_emisor_310_SAF(icf.c_fondo,t_tip_eval,t_tip_fid,t_cod_fid,t_tip_entid,t_cod_entid); -- Informaciòn entidad vigilada.

      --pr_complemento_emisor_310_SAE(fbd_equivalencia_externa('EMPRESA','SAE',icf.eme_emp_codigo,null,null,null,null),
      pr_complemento_emisor_310_SAE(fbd_equivalencia_externa('EMPRESA','SAE',icf.eme_emp_codigo,null,null,null,null),
                                    t_tip_entid_cons,t_cod_entid_cons,t_tip_fid_cons,t_cod_fid_cons);    -- Informaciòn consorcio.

      pr_datos_tipo3_310(t_tip_eval,t_tip_fid,t_cod_fid,t_tip_entid,t_cod_entid,t_tip_entid_cons,t_cod_entid_cons,
                         t_tip_fid_cons,t_cod_fid_cons); --registro tipo 3

      --t_columna := 1 ;
    end if;
    t_fondo_ant := icf.c_fondo;

    --------------------------------------------------------------------------------------
    -- Registro tipo5
    --------------------------------------------------------------------------------------
    t_tipo5     := 0;
    t_tipo5_397 := 0;
    t_subcta := 1;
    if t_borra = 0 then      --- Significa que proceso titulos
    --Busca unicamente los titulos de la empresa del cursor.  Si otra empresa tiene
    --el mismo codigo de fondo, los excluye porque el where solo considera los datos
    --de la empresa enviada por parametro
    for icc in c_titulos(icf.c_fondo,icf.eme_emp_codigo) loop
      --------------------------------------------------------------------------------------
      -- Registro tipo4
      -- se valida si el nit ya fue registrado en el mismop fondo i fidecomiso, si es así
      -- no se debe insertar nuevamente.
      --------------------------------------------------------------------------------------
      begin
        select nvl(max(tmp5_cod_subcta),0)
          into t_max_renglon
          from tmp_reg_tipo1_310
         where tmp5_cod_und_cap = to_number(icc.tmp_unidad_cap)
           and tmp5_fideicomiso = icf.c_fondo
           and tmp1_tipo_reg    = 5
         ;
      exception when no_data_found then
        t_max_renglon := 0;
      end ;

      t_subcta := t_max_renglon + 1 ;

      t_reg := 0;
      select count(*)
        into t_reg
        from tmp_reg_tipo1_310
       where tmp3_tipo_eval  = t_tip_eval
         and tmp3_tipo_fide  = t_tip_fid
         and tmp3_cod_fide   = t_cod_fid
         and tmp3_tipo_ent   = t_tip_entid
         and tmp3_cod_ent    = t_cod_entid
         and tmp4_num_ident  = icc.tmp_c70
         and tmp1_tipo_reg   = 4
      ;

      if t_reg = 0  then
        -- Registro tipo 4 tmp_c72-Tipo ID; tmp_c70-nit; tmp_c71-digito; tmp_c69-Nombre; tmp_c73-Cód emisor
        -- Inicio 13030541
        t_codigo_prv  := icc.tmp_proveedor_pcio;                   -- 12101701
        t_tip_proveed := nvl(substr(t_codigo_prv,1,1),0);          -- 12101701 Tipo Entidad Proveedor de precios
        t_cod_proveed := nvl(substr(t_codigo_prv,2,1),0);          -- 12101701 Código Proveedor de Precios
        -- Fin 13030541
        pr_datos_tipo4_310(t_tip_eval,t_tip_fid,t_cod_fid,t_tip_entid,t_cod_entid,t_tip_entid_cons,t_cod_entid_cons,
                           t_tip_fid_cons,t_cod_fid_cons,icc.tmp_c72,icc.tmp_c70,icc.tmp_c71,icc.tmp_c69,icc.tmp_c73,'0','E'
                          ,t_tip_proveed ,t_cod_proveed);                        -- 12101701
      end if;

      t_columna := 1 ;
      while t_columna <  80 loop
        t_entra := 1;
        t_area_cursor    := dbms_sql.open_cursor;
        t_query := 'select tmp_c' ||t_columna
                  ||' from pfdetalle_formato351_v2 '
                  ||'where tmp_secuen ='|| icc.tmp_secuen
                  ||' union '
                  ||'select tmp_c' ||t_columna
                  ||' from pfdetalle_f351_externo_v2 '
                  ||'where tmp_secuen ='|| icc.tmp_secuen
                           ;
        dbms_sql.parse(t_area_cursor,t_query,0);
        dbms_sql.define_column(t_area_cursor,1,t_nom_col,50);
        -- Ejecutar el cursor
        t_cargar_cursor := dbms_sql.execute(t_area_cursor);
        loop
          if dbms_sql.fetch_rows(t_area_cursor) > 0 then
            dbms_sql.column_value(t_area_cursor, 1,t_nom_col);
          else
            exit;
          end if;
        end loop;
        --Se almacena el valor de la columna 3 para poder validar columnas 4,5 y 6
        if t_columna = 3 then
          t_valcol3 := t_nom_col;
        end if;
        --Si en la columnas 3 el valor reportado es 3 no se debe reportar las columnas 4,5 o 6
        if t_columna = 4 or t_columna = 5 or t_columna = 6 then
          if t_valcol3 = '3' then
            t_entra := 0;
          end if;
        end if;
        --Si el valor de la columna es nula o } no debe ser enviado en el archivo
        if t_nom_col is null or t_nom_col = '}' then
          t_entra := 0;
        end if;
        if (t_columna = 31 or t_columna = 60 or t_columna = 14) and t_nom_col = '0' and icc.tmp_unidad_cap <> 4 and icc.tmp_unidad_cap <> 9 then
          t_entra := 0;
        end if;

        --Si la empresa es aseguradora las columnas 53,54,60 y 63 no se deben reportar
        t_asegura := substr(nvl(fbd_equivalencia_externa('EMPRESA'
                                                    ,'SAS'
                                                    ,icc.tmp_c74
                                                    ,null,null,null,null)
                                                    ,null),1,1);
        if t_columna = 53 or t_columna = 54 or t_columna = 60 or t_columna = 63 then
          if t_asegura = 'N' then
            t_entra := 0;
          end if;
        end if;

        -- Si columnas 38 y 40 no encuentran valor y viene en cero, no reportar columna.
        -- Caso 06101310 cgv 2006-11-14 Act. versiòn estandar con el caso No 6110714
        -- if (t_columna = 38 or t_columna = 40) and ltrim(rtrim(t_nom_col)) = '0' then
        if (t_columna = 40) and ltrim(rtrim(t_nom_col)) = '0' then
          t_entra := 0;
        end if;

        if t_columna = 57 or t_columna = 56 then
          t_nom_col := '}'||t_nom_col;
        end if;

        if t_entra = 1 then
          --se busca el consecutivo en el que va para la unidad de captura, es decir
          --que el consecutivo se mueve por unidad de captura
          t_tipo5 := 1;

          if t_columna not in ('69','70','71','72','73','74') then
            if t_columna = '75' then
              pr_datos_tipo5_310(351,'69',icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            elsif t_columna = '76' then
              pr_datos_tipo5_310(351,'70',icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            elsif t_columna = '77' then
              pr_datos_tipo5_310(351,'71',icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            elsif t_columna = '78' then
              pr_datos_tipo5_310(351,'72',icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            elsif t_columna = '79' then
              pr_datos_tipo5_310(351,'73',icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            else
              pr_datos_tipo5_310(351,t_columna,icc.tmp_unidad_cap,t_subcta,t_nom_col,icf.c_fondo);
            end if;
          end if;

        end if;

        --Cierra el cursor
        dbms_sql.close_cursor(t_area_cursor);
        t_columna := t_columna +1;
      end loop; --final while de columnas
    end loop; --final loop titulos
    ---
    end if; --- del proceso de titulos
    ---
    --- Inicia codigo que procesa formato 397
    ---
    if t_borra_1 = 0 then    ---- Significa que proceso al menos un repo o simultanea
    ---
    for icc in c_titulos397(icf.c_fondo) loop
    ---
      --------------------------------------------------------------------------------------
      -- Registro tipo4
      -- se valida si el nit ya fue registrado en el mismop fondo i fidecomiso, si es así
      -- no se debe insertar nuevamente.
      --------------------------------------------------------------------------------------
      begin
        select nvl(max(tmp5_cod_subcta),0)
          into t_max_renglon
          from tmp_reg_tipo1_310
         where tmp5_cod_und_cap = to_number(icc.tmp_unidad_cap)
           and tmp5_fideicomiso = icf.c_fondo
           and tmp1_tipo_reg    = 5
         ;
         exception when no_data_found then
           t_max_renglon := 0;
      end ;

      t_subcta    := t_max_renglon + 1 ;

      if nvl(t_fon_ant,'0') <> icf.c_fondo then   --- Cambio de fondo y debe crearse un registro tipo 4
        pr_datos_tipo4_310(0,0,0,0,0,0,0,0,0,0,       --- Tipo Identificacion
                           0,                         --- Nit
                           0,                         --- Digito
                           0,                         --- Nombre
                           0,                         --- Codigo Emisor
                           '2',                       --- Tipo Reporte    --- LAA 20080118
                           'T');                      --- tercero
        t_fon_ant := icf.c_fondo;
      end if;
      ---
      t_columna := 1 ;
      --------------------------------------------------------------------------------------
      --- Registro tipo5
      --------------------------------------------------------------------------------------
      ---- Recorre las columnas de la tabla para generar un registro por cada columna si aplica
      while t_columna <  37 loop
         pr_datos_formato_320(t_columna,'pfdetalle_formato397_v2',icc.tmp_secuen,397,icc.tmp_unidad_cap,t_subcta,icf.c_fondo);
         t_columna := t_columna + 1;
      end loop; --final while de columnas
       ---
      t_subcta := t_subcta + 5;
     ----
    end loop;   --final loop titulos 397
    ---
    end if;  -- si proceso repos o simultaneas
    ---
    --- Final codigo que procesa formato 397
    ---
    ant_fideicomiso := icf.c_fondo ;

    -------------------------------------------------------------------------------------
    --- Si no inserto información del formato debe registrar el tipo de registro sin datos.
    --- un registro, dado que si no hay registros debe reportarse el encabezado tipo 3 y 4.
    --------------------------------------------------------------------------------------
    --LAR 2006.07.29
    --Se cambia el insert de los registros tipo 4 y 5 dentro del LOOP de FONDOS
    if t_tipo5 = 0 then
      -- Registro tipo 4 tmp_c72-Tipo ID; tmp_c70-nit; tmp_c71-digito; tmp_c69-Nombre; tmp_c73-Cód emisor
      -- Inicio 12101701 Se trae el proveedor de precios definido para el segmento de la empresa, pues no se tiene en contexto el del título
      t_codigo_prv  := pkg_segmentos.fn_proveedor_segmento(t_emp_cod);           -- 12101701  -- 13030541
      t_tip_proveed := nvl(substr(t_codigo_prv,1,1),0);                          -- 12101701 Tipo Entidad Proveedor de precios
      t_cod_proveed := nvl(substr(t_codigo_prv,2,1),0);                          -- 12101701 Código Proveedor de Precios
      -- Fin 12101701
      pr_datos_tipo4_310(t_tip_eval,t_tip_fid,t_cod_fid,t_tip_entid,t_cod_entid,t_tip_entid_cons,t_cod_entid_cons,
                         t_tip_fid_cons,t_cod_fid_cons,'0',null,null,null,null,'0','E'
                        ,t_tip_proveed ,t_cod_proveed);                          -- 12101701
      pr_datos_tipo5_310(351,99,04,999999,0,ant_fideicomiso);
    end if;

    Begin
      select count(*)
        into t_tipo5_397
        from tmp_reg_tipo1_310
       where tmp1_tipo_reg = 5
         and tmp5_cod_formato = '397'
         and tmp5_fideicomiso = ant_fideicomiso;
      exception when others then t_tipo5_397 := 0;
    End;

    if t_tipo5_397 = 0 then
        -- Registro tipo 4 tmp_c5-Tipo ID; tmp_c6-nit; tmp_c6-digito; tmp_c69-Nombre; tmp_c73-Cód emisor
        pr_datos_tipo4_310(0,0,0,0,0,0,0,0,0,0,       --- Tipo Identificacion
                           0,                         --- Nit
                           0,                         --- Digito
                           0,                         --- Nombre
                           0,                         --- Codigo Emisor
                           '2',                       --- Tipo Reporte
                           'T');                      --- tercero
        pr_datos_tipo5_310(397,99,04,999999,0,ant_fideicomiso);
    end if;

  end loop;  --fondos

  if t_borra_1 = 1 and t_formato = '351' then                 --ACO 20080109
    pr_datos_tipo5_310(397,99,04,999999,0,ant_fideicomiso);
  end if;

  select count(*)
    into t_tot_reg
    from tmp_reg_tipo1_310
  ;

  update tmp_reg_tipo1_310
     set tmp1_tot_reg = t_tot_reg + 1
   where tmp1_tipo_reg = 1
   ;
  select pfseq_tmp_310.nextval
    into t_secuencia5
    from dual;

  insert into tmp_reg_tipo1_310(tmp1_secuencia
                           ,tmp1_tipo_reg
                           ,tmp6_tot_reg
                           )
                     values(t_secuencia5
                           ,6
                           ,t_tot_reg +1
                           )
                          ;


end;
/



  set linesize 100

-------------------------------------------------------------------------------
-- Definicion de columnas de la interfaz
-------------------------------------------------------------------------------
  column linea1           format a49
  column linea2           format a85

--create table pftmp_log as
--select * from tmp_reg_tipo1_310;

  --Registro tipo1
select decode(tmp1_tipo_reg,1,(lpad(tmp1_secuencia,8,0)      ||
                                lpad(nvl(tmp1_tipo_reg,0),1,0)      ||
                                lpad(tmp1_tipo_ent,3,0)             ||
                                lpad(tmp1_cod_entid,6,0)            ||
                                to_char(tmp1_fecha,'ddmmyyyy')      ||
                                lpad(tmp1_tot_reg,8,0)              ||
                                rpad(tmp1_clave,11,' ')             ||
                                lpad(tmp1_area_info,2,0)            ||
                                lpad(tmp1_tipo_inf,2,0)))           linea1
  from tmp_reg_tipo1_310
 where tmp1_tipo_reg = 1
 order by tmp1_secuencia
 ;


  select decode(tmp1_tipo_reg,3,lpad(tmp1_secuencia,8,0)            ||
                                tmp1_tipo_reg                       ||
                                tmp3_tipo_eval                      ||
                                lpad(tmp3_tipo_fide,2,0)            ||
                                lpad(tmp3_cod_fide,6,0)             ||
                                lpad(tmp3_tipo_ent,3,0)             ||
                                lpad(tmp3_cod_ent,6,0)              ||
                                lpad(tmp3_tipo_ent_cons,3,0)        ||
                                lpad(tmp3_cod_ent_cons,6,0)         ||
                                lpad(tmp3_tipo_fide_cons,2,0)       ||
                                lpad(tmp3_cod_fide_cons,6,0)
                             ,4,lpad(tmp1_secuencia,8,0)            ||
                                tmp1_tipo_reg                       ||
                                tmp4_tipo_ident                     ||
                                lpad(tmp4_num_ident,15,0)           ||
                                lpad(tmp4_dig_chequeo,1,0)          ||
                                rpad(upper(tmp4_nombre),50,0)       ||
                                lpad(tmp4_cod_ciiu,4,0)             ||
                                tmp4_cod_natu                       ||
                                tmp4_cam_var1                       ||
                                tmp4_cam_var2                       ||
                                tmp4_cam_var3                       ||
                                tmp4_cod_repor
                             ,5,lpad(tmp1_secuencia,8,0)            ||
                                tmp1_tipo_reg                       ||
                                lpad(tmp5_cod_formato,3,0)          ||
                                lpad(tmp5_cod_columna,2,0)          ||
                                lpad(tmp5_cod_und_cap,2,0)          ||
                                lpad(tmp5_cod_subcta,6,0)           ||
                                tmp5_signo                          ||
                                decode(fn_es_numerico_310(tmp5_valor)
                                      ,1,decode(tmp5_valor
                                               ,null,null
                                                    ,decode(tmp5_cod_formato
                                                           ,351,decode(tmp5_cod_columna
                                                                      -- Alfanumericos. Adicionar la col 1,2,46,61,64,66
                                                                      ,1,lpad(tmp5_valor,50,'}')
                                                                      ,2,lpad(tmp5_valor,50,'}')
                                                                      ,5,lpad(tmp5_valor,50,'}')
                                                                      ,7,lpad(tmp5_valor,50,'}')
                                                                      ,46,lpad(tmp5_valor,50,'}')
                                                                      ,51,lpad(tmp5_valor,50,'}')
                                                                      ,61,lpad(tmp5_valor,50,'}')
                                                                      ,64,lpad(tmp5_valor,50,'}')
                                                                      ,66,lpad(tmp5_valor,50,'}')
                                                                      ,72,lpad(tmp5_valor,50,'}')
                                                                      -- Columnas con 2 decimales
                                                                      ,17,fbd_decimales(tmp5_valor,2,24)
                                                                      ,19,fbd_decimales(tmp5_valor,2,24)
                                                                      ,20,fbd_decimales(tmp5_valor,2,24)
                                                                      ,22,fbd_decimales(tmp5_valor,2,24)
                                                                      ,23,fbd_decimales(tmp5_valor,2,24)
                                                                      ,30,fbd_decimales(tmp5_valor,2,24)
                                                                      ,31,fbd_decimales(tmp5_valor,2,24)
                                                                      ,32,fbd_decimales(tmp5_valor,2,24)
                                                                      ,43,fbd_decimales(tmp5_valor,2,24)
                                                                      ,45,fbd_decimales(tmp5_valor,2,24)
                                                                      ,48,fbd_decimales(tmp5_valor,2,24)
                                                                      ,58,fbd_decimales(tmp5_valor,2,24)
                                                                      ,65,fbd_decimales(tmp5_valor,2,24)
                                                                      ,67,fbd_decimales(tmp5_valor,2,24)
                                                                      ,59,fbd_decimales(tmp5_valor,2,24)
                                                                      ,62,fbd_decimales(tmp5_valor,2,24)
                                                                      -- Columnas con 4 decimales
                                                                      ,24,fbd_decimales(tmp5_valor,4,24)
                                                                      ,25,fbd_decimales(tmp5_valor,4,24)
                                                                      ,33,fbd_decimales(tmp5_valor,4,24)
                                                                      ,36,fbd_decimales(tmp5_valor,4,24)
                                                                      ,37,fbd_decimales(tmp5_valor,4,24)
                                                                      ,38,fbd_decimales(tmp5_valor,4,24)
                                                                      ,39,fbd_decimales(tmp5_valor,4,24)
                                                                      ,75,fbd_decimales(tmp5_valor,4,24)
                                                                      ,76,fbd_decimales(tmp5_valor,4,24)
                                                                      ,79,fbd_decimales(tmp5_valor,4,24)
                                                                      -- Columnas con 6 decimales
                                                                      ,40,fbd_decimales(tmp5_valor,6,24)
                                                                      -- Columnas sin decimales
                                                                         ,fbd_decimales(tmp5_valor,0,24)
                                                                      )
                                                           ,397,decode(tmp5_cod_columna
                                                                      -- Alfanumericos
                                                                      ,2,lpad(tmp5_valor,50,'}')
                                                                      ,3,lpad(tmp5_valor,50,'}')
                                                                      ,6,lpad(tmp5_valor,50,'}')
                                                                      ,7,lpad(tmp5_valor,50,'}')
                                                                      ,21,lpad(tmp5_valor,50,'}')
                                                                      ,24,lpad(tmp5_valor,50,'}')
                                                                      ,26,lpad(tmp5_valor,50,'}')
                                                                      ,28,lpad(tmp5_valor,50,'}')
                                                                      ,32,lpad(tmp5_valor,50,'}')
                                                                      ,33,lpad(tmp5_valor,50,'}')
                                                                      -- Columnas con 2 decimales
                                                                      --12071601,12,fbd_decimales(tmp5_valor,2,24) -- 12060230-12072714
                                                                      ,12,fbd_decimales(tmp5_valor,2,24)           -- 12060230-12072714
                                                                      ,13,lpad(tmp5_valor,50,'}')
                                                                      ,14,fbd_decimales(tmp5_valor,2,24)
                                                                      --12071601,15,fbd_decimales(tmp5_valor,2,24) -- 12060230-12072714
                                                                      ,15,fbd_decimales(tmp5_valor,4,24)           -- 12060230-12072714
                                                                      ,16,lpad(tmp5_valor,50,'}')
                                                                      ,17,fbd_decimales(tmp5_valor,2,24)
                                                                      ,18,lpad(tmp5_valor,50,'}')
                                                                      ,19,fbd_decimales(tmp5_valor,2,24)
                                                                      ,20,fbd_decimales(tmp5_valor,2,24)
                                                                      ,22,fbd_decimales(tmp5_valor,2,24)
                                                                      ,23,fbd_decimales(tmp5_valor,2,24)
                                                                      ,25,fbd_decimales(tmp5_valor,2,24)
                                                                      -- Columnas con 4 decimales
                                                                      ,13,fbd_decimales(tmp5_valor,4,24)
                                                                      ,14,fbd_decimales(tmp5_valor,4,24)
                                                                      -- Columnas sin decimales
                                                                         ,fbd_decimales(tmp5_valor,0,24)
                                                                      )
                                                          )
                                               )
                                        ,decode(tmp5_cod_formato
                                               ,351,lpad(tmp5_valor,50,'}')
                                               ,397,decode(tmp5_cod_columna
                         -- Columnas con formato fecha
                            ,9,lpad(to_char(to_date(tmp5_valor),'ddmmyyyy'),24,'0')
                            ,10,lpad(to_char(to_date(tmp5_valor),'ddmmyyyy'),24,'0')
                            ,11,lpad(to_char(to_date(tmp5_valor),'ddmmyyyy'),24,'0')
                               ,lpad(tmp5_valor,50,'}')
                            )
                 )
                                      )
                             ,6,lpad(tmp1_secuencia,8,0)            ||
                                '6'
                             )                                      linea2
from tmp_reg_tipo1_310
where tmp1_tipo_reg > 1
order by tmp1_secuencia
/
spool off
Declare
 t_registros number;
begin
	-- Se consulta el total de registros
	 select count(*)
	   into t_registros
	   from tmp_reg_tipo1_310;
	-- Se actualiza el log temporal
	 insert into porfin_p.pftmp_log  (log_reporte ,log_registros)
	                 values (  'PFICO310', t_registros);

end;
/

@@id1pfliblog.sql


exit

