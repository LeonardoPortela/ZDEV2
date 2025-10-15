FUNCTION ZPM_GET_DATA_MARC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      MARC STRUCTURE  MARC OPTIONAL
*"----------------------------------------------------------------------

DATA: lr_werks TYPE RANGE OF werks_d.

SELECT * FROM tvarvc INTO TABLE @DATA(lt_tvarv)
  WHERE name EQ 'Z_INDICADORES_FILIAL'
    AND TYPE EQ 'S'.

lr_werks = VALUE #( FOR ls_tvarv IN lt_tvarv ( sign   = ls_tvarv-sign
                                               option = ls_tvarv-opti
                                               low    = ls_tvarv-low
                                               high   = ls_tvarv-high ) ).

SELECT * FROM marc into TABLE marc
  WHERE werks IN lr_werks.

ENDFUNCTION.
