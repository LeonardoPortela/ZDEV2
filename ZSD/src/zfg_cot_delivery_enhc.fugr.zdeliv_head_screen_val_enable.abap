FUNCTION zdeliv_head_screen_val_enable.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(IM_LFART) TYPE  LFART
*"     REFERENCE(IM_VSTEL) TYPE  VSTEL
*"     REFERENCE(IM_WERKS) TYPE  EMPFW
*"  EXPORTING
*"     REFERENCE(EX_MOSTRAR) TYPE  CHAR1
*"----------------------------------------------------------------------

  CONSTANTS c_z_cot_vstel_werks TYPE rvari_vnam VALUE 'Z_COT_VSTEL_WERKS' ##NO_TEXT.
  CONSTANTS c_z_cot_lfart TYPE rvari_vnam VALUE 'Z_COT_LFART' ##NO_TEXT.

  CLEAR ex_mostrar.

  "Seteo por STVARV parametro Z_COT_LFART
  SELECT CAST( low AS CHAR( 4 ) ) AS lfart
    INTO TABLE @DATA(lt_lfart)
    FROM tvarvc
    WHERE name EQ @c_z_cot_lfart.
  IF sy-subrc NE 0.
    RETURN.
  ELSE.
    READ TABLE lt_lfart WITH KEY lfart = im_lfart
                        TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      RETURN.

    ENDIF.
  ENDIF.

  "Seteo por STVARV parametro Z_COT_VSTEL_WERKS
  SELECT CAST( low AS CHAR( 4 ) ) AS vstel,
         CAST( high AS CHAR( 4 ) ) AS werks
    INTO TABLE @DATA(lt_vstel_werks)
    FROM tvarvc
    WHERE name EQ @c_z_cot_vstel_werks.
  IF sy-subrc NE 0.
    RETURN.
  ELSE.
    READ TABLE lt_vstel_werks WITH KEY vstel = im_vstel
*                                         werks = iS_likp-werks
                              TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
  ENDIF.

  ex_mostrar = 'X'.

ENDFUNCTION.
