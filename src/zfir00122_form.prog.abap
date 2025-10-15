
FORM lboxtipo .
  DATA:
    nmtipo  TYPE vrm_id,
    ltipo   TYPE vrm_values,
    vlrtipo TYPE vrm_value.

  nmtipo = 'LBOXTIPO'. " Name should be in UPPER CASE

  vlrtipo-key = 'C'.
  vlrtipo-text = 'Cliente'.
  APPEND vlrtipo TO ltipo.

  vlrtipo-key = 'F'.
  vlrtipo-text = 'Fornecedor'.
  APPEND vlrtipo TO ltipo.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = nmtipo
      values          = ltipo
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.
ENDFORM.

FORM lboxupar .
  DATA:
    nmupar  TYPE vrm_id,
    lupar   TYPE vrm_values,
    vlrupar TYPE vrm_value.

  nmupar = 'LBOXUPAR'. " Name should be in UPPER CASE

  vlrupar-key = 'OVPEDJ'.
  vlrupar-text = 'O.V/Pedido'.
  APPEND vlrupar TO lupar.

  vlrupar-key = 'DCSIMJ'.
  vlrupar-text = 'O.V/Primária'.
  APPEND vlrupar TO lupar.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = nmupar
      values          = lupar
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.
ENDFORM.

FORM lboxtdoc .
  DATA:
    nmtdoc  TYPE vrm_id,
    ltdoc   TYPE vrm_values,
    vlrtdoc TYPE vrm_value.

  nmtdoc = 'LBOXTDOC'. " Name should be in UPPER CASE

  vlrtdoc-key = 'AB'.
  "vlrtdoc-text = 'O.V/Pedido'.
  APPEND vlrtdoc TO ltdoc.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = nmtdoc
      values          = ltdoc
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.
ENDFORM.
