-------------------------------------------------------------------------------
Prompt Compilando fbd_posicion_neteo
-------------------------------------------------------------------------------
Create or Replace
function fbd_posicion_neteo (psn_titulo       number
                            ) return number
is
  ---------------------------------------------------------------------------------------------
  -- Modificaciones:
  --  CASO     CONSULTOR   FECHA      DESCRIPCION CAMBIO
  -- 12031415  LFH       2012/05/29  Se tiene en cuenta al momento de hacer cierre de posiciones el título
  --                                 con consecutivo menor de largos y cortos para registrar en la contabilidad
  --                                 la transacción  utilidad o pérdida en este título, se cambia toda la función
  --
  --
  ---------------------------------------------------------------------------------------------

  t_titulo_largo pftitulos_v2.tit_consec%type;
  t_titulo_corto pftitulos_v2.tit_consec%type;
--------------------------------------------------------------------------
begin -- de fbd_posicion_neteo
  -- Se busca el menor consecutivo de título tanto de Largos (compras) como de Cortos (ventas)
  begin
    select min(net_titulo_compra)  largo
          ,min(net_titulo_venta)   corto
      into t_titulo_largo
          ,t_titulo_corto
      from pfneteo_v2
     where net_consec in(select nvl(max(net_consec),psn_titulo) -- Consecutivo del neteo
                           from pfneteo_v2
                          where (net_titulo_venta = psn_titulo  or net_titulo_compra = psn_titulo));
  exception when others then
    t_titulo_largo := null;
    t_titulo_corto := null;
  end;
  -- Si ambos estan en null puede que no haga parte de un neteo, se busca por el papa
  if t_titulo_largo is null and t_titulo_corto is null then
    begin
      select min(net_titulo_compra)  largo
            ,min(net_titulo_venta)      corto
        into t_titulo_largo
            ,t_titulo_corto
        from pfneteo_v2
            ,pftitulos_v2
       where (net_titulo_venta      = tit_tit_consec  or net_titulo_compra = tit_tit_consec)
         and tit_consec = psn_titulo;
     exception when others then
       t_titulo_largo := null;
       t_titulo_corto := null;
     end;
  end if;
  -- Retornamos el menor de los consecutivos de los títulos (compras o ventas, Largos o Cortos)
  -- Si estos consecutivos no se encontraron, devuelve el ingresado como parámetro (psn_titulo).
  return nvl(least(t_titulo_largo,t_titulo_corto),psn_titulo);
end fbd_posicion_neteo;
----------------------------------------------------------------------------





/
-------------------------------------------------------------------------------
Show err
