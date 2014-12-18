-------------------------------------------------------------------------------
Prompt Compilando fbd_f_anterior_flujo
-------------------------------------------------------------------------------
Create or Replace
FUNCTION            "FBD_F_ANTERIOR_FLUJO" (pfa_titulo number
                               ,pfa_fecha date
                               ,pfa_fecha_default date default null) return date is
  -----------------------------------------------------------------------------
  -- Retorna la fecha de la operacion precendente a la especificada por
  -- pfa_fecha. Sino se retorna la fecha pfa_fecha_default
  -- 14121801 - Justar identacion
  ------------------------------------------------------------------------
   cursor c_oper_ant is
    select ope_fecha fecha
      from pfoperaciones_v2
     where ope_top_codigo||'' in ('0','4','5','Y')
       and ope_fecha+0      < pfa_fecha
       and ope_tit_consec = pfa_titulo
     order by ope_fecha desc
   ;


   t_fecha date := null;
  -----------------------------------------------------------------------------
  Begin -- De fbd_f_anterior_flujo
    for ica in c_oper_ant loop
      return(ica.fecha);
    end loop;
    -- Si no encuentra retorna la fecha enviada por defecto.
    return(pfa_fecha_default);
  End;  -- De fbd_f_anterior_flujo
-------------------------------------------------------------------------------






/
-------------------------------------------------------------------------------
Show err
