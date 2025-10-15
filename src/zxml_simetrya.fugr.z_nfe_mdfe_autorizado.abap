FUNCTION z_nfe_mdfe_autorizado.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_DOCNUM) TYPE  J_1BDOCNUM
*"     REFERENCE(P_USO) TYPE  CHAR01 DEFAULT 'X'
*"  EXPORTING
*"     REFERENCE(P_AUTORIZADO) TYPE  CHAR01
*"  CHANGING
*"     VALUE(P_CABEC) TYPE  J_1BNFDOC OPTIONAL
*"     VALUE(P_ACTIVE) TYPE  J_1BNFE_ACTIVE OPTIONAL
*"  EXCEPTIONS
*"      CANCELADO
*"      NAO_CANCELADO
*"      PENDENTE
*"      NAO_CONCLUIDO
*"      NAO_EXISTE
*"      AUTORIZADO_USO
*"      DENEGADO
*"      MDFE_AUTORIZADO
*"----------------------------------------------------------------------

  DATA: it_doc_text TYPE TABLE OF dd07v WITH KEY domvalue_l,
        it_doc_requ TYPE TABLE OF dd07v WITH KEY domvalue_l,
        wa_doc_text TYPE dd07v.

  CLEAR: p_autorizado.

  CHECK NOT p_docnum IS INITIAL. "DOCNUM CTE não está vazio

  "Recuperar MDFE

  SELECT *
         INTO TABLE  @DATA(it_mdfe)
         FROM zsdt0105
        WHERE  docnum EQ @p_docnum.

  LOOP AT it_mdfe ASSIGNING FIELD-SYMBOL(<fs_mdfe>).

    SELECT SINGLE   nmdfe , docnum, contingencia
        FROM zsdt0102
      INTO (@DATA(lv_nfnum), @DATA(lv_docnum_mdfe), @DATA(lv_contingencia) ) "*-CONTINGENCIA MDF-E - JT - 06.05.2024 ========
        WHERE docnum EQ @<fs_mdfe>-docnum_ref
      AND autorizado EQ 'X'
      AND estornado EQ ' '
      AND cancel EQ ' ' .

    IF sy-subrc EQ 0.
      EXIT.
    ENDIF.

  ENDLOOP.


  IF NOT lv_docnum_mdfe IS INITIAL. "**MDFE EXISTE

*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    IF lv_contingencia = abap_true.
      p_autorizado = c_x.
      RETURN.
    ENDIF.
*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    SELECT SINGLE * INTO p_active
      FROM j_1bnfe_active
     WHERE docnum EQ lv_docnum_mdfe.

    IF sy-subrc IS INITIAL.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = 'J_1BNFEDOCSTATUS'
          state         = 'A'
          langu         = sy-langu
        TABLES
          dd07v_tab     = it_doc_text
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        MESSAGE ID 'J1B_NFE' TYPE 'W' NUMBER '025'.
      ENDIF.

      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = 'J_1BNFE_ACTION_REQUIRED'
          state         = 'A'
          langu         = sy-langu
        TABLES
          dd07v_tab     = it_doc_requ
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.

      IF sy-subrc <> 0.
        MESSAGE ID 'J1B_NFE' TYPE 'W' NUMBER '025'.
      ENDIF.

      " 'X' Documento autorizado ao uso na SEFAZ
      " ' ' Documento com cancelamento autorizado na SEFAZ
      " 'N' Documento não autorizado e permitido alteração

      IF p_uso EQ c_x.

        IF p_active-docsta EQ c_1 AND p_active-cancel IS INITIAL AND p_cabec-cancel IS INITIAL AND
           p_active-action_requ EQ c_c.
          p_autorizado = c_x.
        ELSEIF p_active-docsta EQ c_1 AND p_active-cancel IS INITIAL AND p_cabec-cancel IS INITIAL.
          READ TABLE it_doc_requ INTO wa_doc_text WITH KEY domvalue_l = p_active-action_requ.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext RAISING nao_concluido.
        ELSEIF ( NOT p_active-cancel IS INITIAL ) OR ( NOT p_cabec-cancel IS INITIAL ).
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe 'cancelado!' RAISING cancelado.
        ELSEIF p_active-docsta NE '1'.
          READ TABLE it_doc_text INTO wa_doc_text WITH KEY domvalue_l = p_active-docsta.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext 'Pendente de Autorização' RAISING pendente.
        ENDIF.

      ELSEIF p_uso IS INITIAL.

        IF p_active-docsta EQ c_1 AND p_active-cancel IS NOT INITIAL AND p_cabec-cancel IS NOT INITIAL AND
          p_active-action_requ EQ c_c.
          p_autorizado = c_x.
        ELSEIF p_active-docsta EQ c_1 AND p_active-cancel IS NOT INITIAL AND p_cabec-cancel IS NOT INITIAL.
          READ TABLE it_doc_requ INTO wa_doc_text WITH KEY domvalue_l = p_active-action_requ.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext RAISING nao_concluido.
        ELSEIF ( p_active-cancel IS INITIAL ) OR ( p_cabec-cancel IS INITIAL ).
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe 'não cancelado!' RAISING nao_cancelado.
        ELSEIF p_active-docsta NE '1'.
          READ TABLE it_doc_text INTO wa_doc_text WITH KEY domvalue_l = p_active-docsta.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext 'Pendente de Autorização' RAISING pendente.
        ENDIF.

      ELSEIF p_uso EQ c_n.

        IF p_active-docsta EQ c_1 AND p_active-cancel IS INITIAL AND p_cabec-cancel IS INITIAL AND
           p_active-action_requ EQ c_c.
          "Autoriza para uso
          READ TABLE it_doc_requ INTO wa_doc_text WITH KEY domvalue_l = p_active-action_requ.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext RAISING autorizado_uso.
        ELSEIF p_active-docsta EQ c_1 AND p_active-cancel IS NOT INITIAL AND p_cabec-cancel IS NOT INITIAL AND
          p_active-action_requ EQ c_c.
          "Autorizado o cancelamento
          READ TABLE it_doc_requ INTO wa_doc_text WITH KEY domvalue_l = p_active-action_requ.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe wa_doc_text-ddtext RAISING autorizado_uso.
        ELSEIF ( NOT p_active-cancel IS INITIAL ) OR ( NOT p_cabec-cancel IS INITIAL ).
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe 'cancelado!' RAISING cancelado.
        ELSEIF ( p_active-docsta NE '1' ) AND ( p_active-docsta NE '2' ) .
          p_autorizado = c_x.
        ELSEIF p_active-docsta EQ '2'.
          MESSAGE e023 WITH 'Documento:' lv_docnum_mdfe 'denegado e não permitido alterar!' RAISING denegado.
        ENDIF.

      ENDIF.

    ELSE.
      "  MESSAGE e023 WITH 'Documento' lv_docnum_mdfe 'não localizado,emitir e autorizar o MDF-e!' RAISING nao_existe.
      MESSAGE e023 WITH 'Para autorizar é necessário que o Mdf-e esteja Autorizado' RAISING nao_existe.
    ENDIF.

  ELSE.
    "Não precisa de autorização
    " p_autorizado = 'X'. Mdf-e ainda não foi gerado
    " MESSAGE e023 WITH 'Documento' lv_docnum_mdfe 'não localizado,emitir e autorizar o MDF-e!' RAISING nao_existe.
    MESSAGE e023 WITH 'Para autorizar é necessário que o Mdf-e esteja Autorizado' RAISING nao_existe.
  ENDIF.

ENDFUNCTION.
