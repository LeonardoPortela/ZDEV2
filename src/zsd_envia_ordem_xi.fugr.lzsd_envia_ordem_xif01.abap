*&---------------------------------------------------------------------*
*&      Form  L_CHECK_ITEM_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ORDEM  text
*      <--P_E_FERT_PRODUCAO  text
*----------------------------------------------------------------------*
FORM f_check_item_material  USING    ws_ordem TYPE zsde0001
                            CHANGING e_fert_producao TYPE char01.

  DATA: ws_mara TYPE mara.

  CLEAR: ws_mara.
  SELECT SINGLE * FROM mara INTO ws_mara WHERE matnr EQ ws_ordem-cd_material.
  IF sy-subrc EQ 0.

    DATA: ws_centro TYPE setleaf.

    "Selecionar dados centros do SET werks_fabrica_fert.
    CLEAR: ws_centro, e_fert_producao.
    SELECT SINGLE *
    FROM setleaf
    INTO ws_centro
      WHERE setname EQ 'MV45AFZZ_WERKS'
       AND valfrom EQ ws_ordem-cd_centro.

    "Validação do material.
    IF ws_mara-mtart EQ 'ZFER' AND ws_mara-spart EQ '02' AND ws_centro IS NOT INITIAL.
      e_fert_producao = abap_true.
    ELSE.
      e_fert_producao = abap_false.
    ENDIF.

  ENDIF.
ENDFORM.
