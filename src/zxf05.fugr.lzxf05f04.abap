*----------------------------------------------------------------------*
***INCLUDE ZXF05F04 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTIR_CNPJ_INSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LFA1_LIFNR  text
*      -->P_I_LFA1_STCD1  text
*      -->P_I_LFA1_STCD3  text
*----------------------------------------------------------------------*
FORM f_consistir_cnpj_insc  USING    p_but000 TYPE but000
                                     p_lifnr
                                     p_stcd1
                                     p_stcd3
                                     p_pergunta.

  DATA: v_lifnr TYPE lfa1-lifnr,
        v_stcd1 TYPE lfa1-stcd1,
        v_stcd3 TYPE lfa1-stcd3,
        v_text1 TYPE spop-textline2,
        v_text2 TYPE spop-textline2,
        v_res   TYPE c LENGTH 1 VALUE 'N',
        v_erro  TYPE c LENGTH 1 VALUE '0',
        v_dup   TYPE c LENGTH 3 VALUE '0'.

  CHECK NOT p_stcd1 IS INITIAL.

*-CS2024000622-19.09.2024-JT-#152691-inicio
*  SELECT lifnr stcd1 INTO (v_lifnr, v_stcd1)
*     FROM lfa1 UP TO 1 ROWS
*     WHERE stcd1 = p_stcd1 AND
*           stcd3 = p_stcd3 AND
*           lifnr <> p_lifnr AND
*           sperr NE 'X'.
*    v_erro = 1.
*  ENDSELECT.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibpsupplier   ON ibpsupplier~businesspartner = dfkkbptaxnum~partner
   INNER JOIN lfa1          ON lfa1~lifnr                  = ibpsupplier~supplier
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd1
     AND taxtype    = 'BR1'
     AND lfa1~sperr = @abap_false
     AND lfa1~loevm = @abap_false
     AND lfa1~nodel = @abap_false.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibpsupplier   ON ibpsupplier~businesspartner = dfkkbptaxnum~partner
   INNER JOIN lfa1          ON lfa1~lifnr                  = ibpsupplier~supplier
APPENDING TABLE @t_tax
   WHERE taxnum     = @p_stcd3
     AND taxtype    = 'BR3'
     AND lfa1~sperr = @abap_false
     AND lfa1~loevm = @abap_false
     AND lfa1~nodel = @abap_false.

  DATA(t_fornece) = t_tax[].

  DELETE t_fornece WHERE partner = p_but000-partner. "p_lifnr.
  DELETE ADJACENT DUPLICATES FROM t_fornece
                        COMPARING partner.

  LOOP AT t_fornece INTO DATA(w_fornece).
    READ TABLE t_tax INTO DATA(w_tax) WITH KEY partner = w_fornece-partner
                                               taxtype = 'BR3'
                                               taxnum  = p_stcd3.
    IF sy-subrc = 0.
      READ TABLE t_tax INTO w_tax WITH KEY partner = w_fornece-partner
                                           taxtype = 'BR1'
                                           taxnum  = p_stcd1.
      IF sy-subrc = 0.
        v_lifnr = w_tax-partner.
        v_stcd1 = w_tax-taxnum.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  IF sy-subrc NE 0.
*    IF sy-tcode(2) NE 'XK'.
*      SELECT lifnr stcd1 INTO (v_lifnr, v_stcd1)
*       FROM lfa1 UP TO 1 ROWS
*       WHERE stcd1 = p_stcd1 AND
*             lifnr <> p_lifnr AND
*             sperr NE 'X'.
*        v_erro = 2.
*      ENDSELECT.
*    ENDIF.
*  ENDIF.
*-CS2024000622-19.09.2024-JT-#152691-fim

* IF sy-subrc = 0.
  IF v_stcd1 IS NOT INITIAL.  "*-CS2024000622-19.09.2024-JT-#152691
    MESSAGE e010(zfi) RAISING error. "CNPJ + Inscrição estadual duplicados !
*
** --->> CS1014779/CS1024167 - IR106423/IR110588 ---->>
*    CONCATENATE 'Código:' v_lifnr '-' v_stcd1 INTO v_text1.
**   CONCATENATE 'CNPJ:' v_stcd1 INTO v_text1.
*    IF v_erro EQ 1.
*      v_text2 = 'CNPJ/INSC.ESTADUAL DUPLICADO - Deseja Realmente Gravar?'.
*    ELSE.
*      v_text2 = 'CNPJ DUPLICADO - Deseja Realmente Gravar?'.
*    ENDIF.
*
*    IF sy-batch = abap_off.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          defaultoption = 'N'
*          titel         = 'ATENÇÃO'
*          textline1     = v_text1
*          textline2     = v_text2
*        IMPORTING
*          answer        = v_res.
*    ELSE.
*      v_res = 'N'.
*    ENDIF.
*
*    IF v_res EQ 'N' OR v_res EQ 'A'.
*      MESSAGE e010(zfi) RAISING error. "CNPJ + Inscrição estadual duplicados !
*    ENDIF.
  ENDIF.

ENDFORM.                    " F_CONSISTIR_CNPJ_INSC

*----------------------------------------------------------------------*
* validar IBAN
*----------------------------------------------------------------------*
FORM f_validar_iban TABLES t_lfbk STRUCTURE lfbk
                     USING p_ktokk.

  TYPES: BEGIN OF ty_lfbk2.
           INCLUDE STRUCTURE lfbk.
  TYPES:   bankn2 TYPE bankn35.
  TYPES: END   OF ty_lfbk2.

  DATA: t_lfbk2 TYPE TABLE OF ty_lfbk2,
        w_lfbk2 TYPE ty_lfbk2,
        t_set   TYPE TABLE OF rgsb4,
        w_set   TYPE rgsb4,
        w_tiban TYPE tiban,
        l_lifnr TYPE lifnr.

  DATA: v_answer   TYPE string,
        v_answer2  TYPE string,
        v_mensagem TYPE string.

  RANGES: r_ktokk FOR lfa1-ktokk.

  FREE: t_set, r_ktokk, t_lfbk2.

  CHECK t_lfbk[] IS NOT INITIAL.

  LOOP AT t_lfbk             INTO DATA(w_lfbk).
    MOVE w_lfbk-lifnr          TO l_lifnr.
    MOVE-CORRESPONDING w_lfbk  TO w_lfbk2.
    MOVE w_lfbk-bankn          TO w_lfbk2-bankn2.
    APPEND w_lfbk2             TO t_lfbk2.
  ENDLOOP.
*-------------------------------------
* busca SET
*-------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_VALIDA_IBAN_KTOKK'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  LOOP AT t_set INTO w_set.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_set-from ) TO r_ktokk.
  ENDLOOP.

  CHECK p_ktokk IN r_ktokk[].

*-------------------------------------
* busca IBAN
*-------------------------------------
  SELECT *
    INTO TABLE @DATA(t_tiban)
    FROM tiban
     FOR ALL ENTRIES IN @t_lfbk2
   WHERE banks = @t_lfbk2-banks
     AND bankl = @t_lfbk2-bankl
     AND bankn = @t_lfbk2-bankn2
     AND bkont = @t_lfbk2-bkont.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM lfbk
      INTO TABLE @DATA(t_lfbk3)
      FOR ALL ENTRIES IN @t_tiban
      WHERE banks = @t_tiban-banks
        AND bankl = @t_tiban-bankl
        AND bankn = @t_tiban-bankn(18)
        AND bkont = @t_tiban-bkont.

    IF sy-subrc IS INITIAL.
      DELETE t_lfbk3 WHERE lifnr = l_lifnr.

      SORT: t_lfbk3 BY banks bankl bankn bkont.

      LOOP AT t_lfbk2 INTO w_lfbk2.

        LOOP AT t_tiban INTO w_tiban WHERE banks = w_lfbk2-banks
                                       AND bankl = w_lfbk2-bankl
                                       AND bankn = w_lfbk2-bankn2
                                       AND bkont = w_lfbk2-bkont.

          READ TABLE t_lfbk3 INTO DATA(lwa_lfbk)
                             WITH KEY banks = w_tiban-banks
                                      bankl = w_tiban-bankl
                                      bankn = w_tiban-bankn(18)
                                      bkont = w_tiban-bkont BINARY SEARCH.
          CHECK sy-subrc = 0.

          SELECT SINGLE ktokk
            FROM lfa1
            INTO @DATA(lv_ktokk)
            WHERE lifnr = @lwa_lfbk-lifnr.

          CONCATENATE 'Código IBAN:'
                w_tiban-iban
                'já cadastrado para fornecedor :'
                lwa_lfbk-lifnr '-'
                lv_ktokk
                '. Deseja realizar o novo cadastro ?'
                INTO v_mensagem SEPARATED BY space.

          IF sy-batch = abap_off.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar       = 'Mensagem de confirmação'
                text_question  = v_mensagem
                text_button_1  = 'Sim'
                text_button_2  = 'Não'
              IMPORTING
                answer         = v_answer
              EXCEPTIONS
                text_not_found = 1
                OTHERS         = 2.
          ELSE.
            v_answer = '2'.
          ENDIF.

          IF v_answer = '2' OR v_answer = 'A'.
            MESSAGE e024(sd) WITH 'Remover linha com IBAN: ' w_tiban-iban RAISING error.
          ELSEIF v_answer = '1'.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar       = 'Mensagem de confirmação'
                text_question  = 'Tem certeza?'
                text_button_1  = 'Sim'
                text_button_2  = 'Não'
              IMPORTING
                answer         = v_answer2
              EXCEPTIONS
                text_not_found = 1
                OTHERS         = 2.

            IF v_answer2 = '2' OR v_answer2 = 'A'.
              MESSAGE e024(sd) WITH 'Remover linha com IBAN: ' w_tiban-iban RAISING error.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

*-------------------------------------
* valida IBAN duplicado mesmo país
*-------------------------------------
*  READ TABLE t_lfbk INTO w_lfbk INDEX 1.
*
*  LOOP AT t_tiban INTO w_tiban.
*
*    CHECK w_tiban-tabkey <> w_lfbk-lifnr.
*
*    SELECT land1
*      INTO @DATA(l_land1)
*        UP TO 1 ROWS
*      FROM lfa1
*     WHERE lifnr = @w_tiban-tabkey.
*    ENDSELECT.
*
*    CHECK l_land1 IN r_land1[].
*
*    IF p_land1 = l_land1.
*      MESSAGE e024(sd) WITH text-100 w_tiban-iban text-101 w_tiban-tabkey.
*      EXIT.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.

*---------------------------------------------------------------------*
* VALIDA CONTA CONCILIACAO
*---------------------------------------------------------------------*
FORM f_valida_conta USING p_bukrs
                          p_lifnr
                          p_stcd1
                          p_stcd2
                          p_akont.

  SELECT *
    INTO @DATA(w_0190)
    FROM zfit0190
      UP TO 1 ROWS
   WHERE bukrs     = @p_bukrs
     AND cpf       = @p_stcd2
     AND cnpj_raiz = @abap_off.
  ENDSELECT.

  IF sy-subrc = 0 AND p_akont <> w_0190-akont.
    MESSAGE e024(sd) WITH 'Fornecedor parte relacionada. '
                          'Conta conciliação incorreta!' RAISING error.
  ELSE.
    SELECT *
      INTO w_0190
      FROM zfit0190
        UP TO 1 ROWS
     WHERE bukrs     = p_bukrs
       AND cpf       = abap_off
       AND cnpj_raiz = p_stcd1(8).
    ENDSELECT.

    IF sy-subrc = 0 AND p_akont <> w_0190-akont.
      MESSAGE e024(sd) WITH 'Fornecedor parte relacionada. '
                            'Conta conciliação incorreta!' RAISING error.
    ENDIF.
  ENDIF.

  "Verifica se a empresa pode utlizar a conta conciação.
  SELECT *
    FROM zfit0190
     INTO TABLE @DATA(it_0190)
   WHERE akont = @p_akont.
  IF sy-subrc = 0.
    SORT it_0190 BY bukrs.
    DELETE it_0190 WHERE bukrs NE p_bukrs.
    IF it_0190 IS INITIAL.
      MESSAGE e024(sd) WITH 'Conta conciliação->' p_akont 'não permitida para empresa' p_bukrs RAISING error.
    ENDIF.
  ENDIF.


ENDFORM.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
