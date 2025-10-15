*&---------------------------------------------------------------------*
*&  Include           ZLESR0162_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: lv_kunnr TYPE kna1-kunnr,
        lv_lifnr TYPE lfa1-lifnr.

  CLEAR: sy-ucomm.

  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TEXTO'.

  IF lv_zone_des IS INITIAL.
    GET PARAMETER ID 'ZONA_DES' FIELD lv_zone_des.
  ENDIF.

  IF lv_zone_ori IS INITIAL.
    GET PARAMETER ID 'ZONA_ORI' FIELD lv_zone_ori.
  ENDIF.

  IF wa_kna1-kunnr IS INITIAL.
    GET PARAMETER ID 'COD_CLI' FIELD lv_kunnr.
  ENDIF.

  IF wa_lfa1_pc-lifnr IS INITIAL.
    GET PARAMETER ID 'COD_PC' FIELD lv_lifnr.
  ENDIF.

  IF lv_kunnr IS NOT INITIAL.
    wa_kna1-kunnr = lv_kunnr.
  ENDIF.

  IF lv_lifnr IS NOT INITIAL.
    wa_lfa1_pc-lifnr = lv_lifnr.
  ENDIF.

  PERFORM f_preenche_campos.

ENDMODULE.

MODULE pedagio_listbox INPUT. "170657 CS2025000299 CHECKBOX TO LIST ZLES0214 PSA

  FREE: it_f1_sim_nao,list.
  CLEAR: value.

  SELECT
    name,
    "UPPER( name ) as name,
    id FROM zi_f1_sim_nao
    INTO TABLE @it_f1_sim_nao.

    SORT it_f1_sim_nao by id DESCENDING.

  LOOP AT it_f1_sim_nao INTO DATA(wa_f1_sim_nao).
    value-key = wa_f1_sim_nao-name.
    APPEND value TO list.
    CLEAR: wa_f1_sim_nao.
  ENDLOOP.


  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_PEDAGIO'
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.
ENDMODULE.
