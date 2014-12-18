-------------------------------------------------------------------------------
Prompt Compilando fbd_f_fin_mes
-------------------------------------------------------------------------------
Create or Replace
FUNCTION            "FBD_F_FIN_MES" (pff_fecha      date                            -- Fecha de proceso
                      ,pff_f_compra   date                            -- Fecha de compra dada
                      ,pff_f_emision  date                            -- Fecha de emision dada
                      ) return        date is
----------------------------------------------------------------------------
-- Objetivo : Obtener la fecha ultimo dia del mes anterior a la fecha parametro
-- Si la fecha es inferior a la f_compra dada se retorna el ultimo dia del mes de f_compra
----------------------------------------------------------------------------
   -------------------------------------------------------------------------
   -- Variables locales a funcion fbd_f_fin_mes
   -------------------------------------------------------------------------
   t_fecha         date;
----------------------------------------------------------------------------
Begin -- De fbd_f_fin_mes


  t_fecha := last_day(add_months(pff_fecha,-1));
  if t_fecha < pff_f_compra then
    t_fecha := last_day(pff_f_compra);
  end if;


  return(t_fecha);
End;  -- De fbd_f_fin_mes
----------------------------------------------------------------------------






/
-------------------------------------------------------------------------------
Show err
