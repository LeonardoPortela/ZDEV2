FUNCTION z_les_save_text_vt.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TKNUM) TYPE  TKNUM
*"     VALUE(I_PLACA_CAV) TYPE  ZPLACA OPTIONAL
*"     VALUE(I_ID_TEXT) TYPE  TDID DEFAULT 'CM18'
*"----------------------------------------------------------------------

  DATA: ws_zlest0002       TYPE zlest0002,
        ws_zlest0135       TYPE zlest0135,
        lt_lines           TYPE TABLE OF tline,
        lst_lines          LIKE LINE OF  lt_lines,
        vg_desc_tip_transp TYPE char6,
        vg_equiparado      TYPE char6,
        vg_base_tipbank    TYPE char6,
        tl_tlines          LIKE tline OCCURS 0 WITH HEADER LINE,
        st_header          TYPE thead.


  CLEAR: vg_desc_tip_transp, vg_equiparado, ws_zlest0002, ws_zlest0135.

*--------------------------------------------------------------------------------------------------------*
*   "Verificar se existe dados do transportador.
*--------------------------------------------------------------------------------------------------------*

  SELECT SINGLE * FROM zlest0002
    INTO ws_zlest0002
    WHERE pc_veiculo EQ i_placa_cav.
  IF sy-subrc EQ 0.
    SELECT SINGLE * FROM zlest0135
    INTO ws_zlest0135
    WHERE cd_transportador EQ ws_zlest0002-proprietario
    AND ds_placa EQ ws_zlest0002-pc_veiculo.
    IF sy-subrc EQ 0.
*--------------------------------------------------------------------------------------------------------*
*   Preencher tabela de texto.
*--------------------------------------------------------------------------------------------------------*

      CASE ws_zlest0135-tp_transportador.
        WHEN 1. "TAC
          vg_desc_tip_transp = 'TAC'.
        WHEN 2. "ETC
          vg_desc_tip_transp = 'ETC'.
        WHEN 3. "CTC
          vg_desc_tip_transp = 'CTC'.
        WHEN OTHERS.
      ENDCASE.

      IF ws_zlest0135-ck_etc_equiparado EQ abap_true.
        vg_equiparado = 'Sim'.
      ELSE.
        vg_equiparado = 'Não'.
      ENDIF.

      IF ws_zlest0135-consulta_base_tipbank EQ abap_true.
        vg_base_tipbank = 'Sim'.
      ELSE.
        vg_base_tipbank = 'Não'.
      ENDIF.


      FREE: tl_tlines.
      "Prenchendo o tipo de transportador.
      APPEND VALUE #(
        tdline = |Tipo Transportador: { ws_zlest0135-tp_transportador } - { vg_desc_tip_transp }|
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo se o tipo de transportador é equiparado.
      APPEND VALUE #(
        tdline = |Equiparado TAC: { vg_equiparado }|
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo data e hora.
      APPEND VALUE #(
        tdline = |Data atualização: { ws_zlest0135-dt_atualizacao+6(2) }.{ ws_zlest0135-dt_atualizacao+4(2) }.{ ws_zlest0135-dt_atualizacao+0(4) } |
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo data e hora.
      APPEND VALUE #(
        tdline = |Hora atualização: { ws_zlest0135-hr_atualizacao+0(2) }:{ ws_zlest0135-hr_atualizacao+2(2) }:{ ws_zlest0135-hr_atualizacao+4(2) }|
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo se o tipo de consulta foi na base da Tip.
      APPEND VALUE #(
        tdline = |Consulta Base TIPBANK: { vg_base_tipbank }|
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo o grupo tipo transportador.
      APPEND VALUE #(
        tdline = |GRUPO: { ws_zlest0002-grupo }|
      tdformat = |*| ) TO tl_tlines.

      "Prenchendo o grupo tipo transportador.
      APPEND VALUE #(
        tdline = |FROTA: { ws_zlest0002-frota }|
      tdformat = |*| ) TO tl_tlines.


*--------------------------------------------------------------------------------------------------------*
*   Preencher estrutura.
*--------------------------------------------------------------------------------------------------------*

      i_tknum = |{ i_tknum ALPHA = IN }|.

      st_header-tdobject = 'VTTK'.
      st_header-tdname   = i_tknum.
      st_header-tdid     = i_id_text.
      st_header-tdspras  = sy-langu.


      IF ( tl_tlines[] IS NOT INITIAL ).
        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            header          = st_header
            insert          = abap_true
            savemode_direct = abap_true
          TABLES
            lines           = tl_tlines
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.
        IF sy-subrc EQ 0.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.





ENDFUNCTION.
