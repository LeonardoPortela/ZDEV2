"Name: \FU:J_1BDFE_GET_ACTIVE_SERVER\SE:BEGIN\EI
ENHANCEMENT 0 Z_DRC_BYPASS_CK_STATUS_SERVER.

  CHECK cs_header-model <> '58'.

  SELECT SINGLE regio
   FROM j_1bnfe_active
   INTO @DATA(lv_regio_sf)
  WHERE docnum = @cs_header-docnum .

  IF sy-subrc EQ 0 .
    SELECT SINGLE *
      FROM zdrct0005
      INTO @DATA(_zdrct0005)
     WHERE regio  = @lv_regio_sf
       AND model  = @cs_header-model.

    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
