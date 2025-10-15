Clear:
        V_CLPAIS,
        V_CLPROV.

Select single LANDX50
from t005t
into v_CLpais
where LAND1 eq GS_dkadr-LAND1
  and Spras eq 'S'.


Select single BEZEI
from t005U
into v_CLProv
where BLAND eq GS_dkadr-regio
  and Spras eq 'S'
  and LAND1 eq GS_dkadr-LAND1.


















