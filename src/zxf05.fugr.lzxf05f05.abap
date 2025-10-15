*----------------------------------------------------------------------*
***INCLUDE LZXF05F05 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  f_consistir_cpf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIFNR  text
*      -->P_STCD2  text
*----------------------------------------------------------------------*
FORM f_dados_bancarios TABLES t_knbk STRUCTURE knbk
                        USING p_kna1 STRUCTURE kna1. "*-CS2024000622-18.09.2024-JT-#152328-inicio

  IF     p_kna1-kunnr(2) = '##'.
    IF t_knbk[] IS NOT INITIAL.
      MESSAGE e132(zfi) RAISING error.
    ENDIF.
  ELSEIF p_kna1-lifnr IS NOT INITIAL.
*-CS2024000622-18.09.2024-JT-#152328-inicio
    SELECT SINGLE kunnr
      INTO @DATA(_kunnr)
      FROM lfa1
     WHERE lifnr = @p_kna1-lifnr.

    IF sy-subrc <> 0 OR ( sy-subrc = 0 AND _kunnr IS INITIAL ).
      MESSAGE e132(zfi) RAISING error.
    ENDIF.
*-CS2024000622-18.09.2024-JT-#152328-fim

    SELECT SINGLE businesspartner
      INTO @DATA(_businesspartner)
      FROM ibupacustomer
     WHERE customer = @_kunnr.  "*-CS2024000622-18.09.2024-JT-#152328-inicio

    IF sy-subrc = 0.
      SELECT SINGLE supplier
        INTO @DATA(_supplier)
        FROM ibpsupplier
       WHERE businesspartner = @_businesspartner.

      IF sy-subrc <> 0.
        MESSAGE e132(zfi) RAISING error.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_consistir_cpf_cli  USING    p_but000 TYPE but000
                                   p_kunnr
                                   p_stcd2
                       CHANGING    p_mess
                                   p_kunnr2 LIKE kna1-kunnr.

  DATA: v_stcd2 TYPE lfa1-stcd2,
        v_text1 TYPE spop-textline2,
        v_text2 TYPE spop-textline2,
        v_res   TYPE c LENGTH 1 VALUE 'N'.

  CHECK NOT p_stcd2 IS INITIAL.

*-CS2024000622-19.09.2024-JT-#152691-inicio
*  SELECT    kunnr     stcd2
*    INTO (p_kunnr2, v_stcd2)
*    FROM kna1
*   UP TO 1 ROWS
*   WHERE stcd2  = p_stcd2
*     AND kunnr <> p_kunnr.
*  ENDSELECT.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
   INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd2
     AND taxtype    = 'BR2'
     AND kna1~sperr = @abap_false
     AND kna1~loevm = @abap_false
     AND kna1~nodel = @abap_false.

  DATA(t_cliente) = t_tax[].

  DELETE t_cliente WHERE partner = p_but000-partner. "p_kunnr.

  READ TABLE t_cliente INTO DATA(w_cliente) INDEX 1.
*-CS2024000622-19.09.2024-JT-#152691-fim

  IF ( sy-subrc EQ 0 ).
    MESSAGE e024(sd) WITH 'Cpf duplicado ! Parceiro: ' w_cliente-partner RAISING error.
*   MESSAGE e007(zfi) RAISING error. "CPF Duplicado
  ENDIF.

*    IF ( p_mess EQ 'X' ).
** --->> IR110588 / CS1024167 ---->>
*      CONCATENATE 'Código:'  p_kunnr2 '-'v_stcd2 INTO v_text1.
**     CONCATENATE 'CNPJ:' p_stcd2 INTO v_text1.
*      v_text2 = 'CNPJ DUPLICADO - Deseja Realmente Gravar?'.
*
*      IF sy-batch = abap_off.
*        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*          EXPORTING
*            defaultoption = 'N'
*            titel         = 'ATENÇÃO'
*            textline1     = v_text1
*            textline2     = v_text2
*          IMPORTING
*            answer        = v_res.
*      ELSE.
*        v_res = 'N'.
*      ENDIF.
*
*      IF v_res EQ 'N' OR v_res EQ 'A'.
*        IF ( p_mess EQ 'X' ).
*          MESSAGE e007(zfi) RAISING error. "CPF Duplicado
*        ELSE.
*          p_mess = 'E'.
*        ENDIF.
*      ENDIF.
** <<--- IR110588 / CS1024167 <<----
*    ELSE.
*      p_mess = 'E'.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " f_consistir_cpf

FORM f_consistir_cnpj_cli  USING    p_but000 TYPE but000
                                    p_kunnr
                                    p_stcd1
                                    p_stcd3
                                    p_regio
                                    p_ktokd
                          CHANGING  p_mess
                                    p_kunnr2 LIKE kna1-kunnr.

  DATA: v_stcd1    TYPE kna1-stcd1,
        v_pergunta TYPE c LENGTH 1,
        v_text1    TYPE spop-textline2,
        v_text2    TYPE spop-textline2,
        v_res      TYPE c LENGTH 1,
        v_res1     TYPE c LENGTH 1,
        vstate     TYPE setleaf-valfrom,
        e_flg      TYPE char01,
        e_kunnr    TYPE kna1-kunnr.

  CHECK NOT p_stcd1 IS INITIAL.

  SELECT SINGLE valfrom
    FROM setleaf
    INTO vstate
   WHERE setname = 'MAGGI_FORN_DUPL'
     AND valfrom = p_regio .

  IF sy-subrc = 0.
    v_pergunta = abap_true.
  ENDIF.

*-CS2024000622-19.09.2024-JT-#152691-inicio
*  SELECT    kunnr     stcd1
*    INTO (p_kunnr2, v_stcd1)
*    FROM kna1
*   UP TO 1 ROWS
*   WHERE stcd1 = p_stcd1
*     AND kunnr <> p_kunnr.
*  ENDSELECT.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
   INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd1
     AND taxtype    = 'BR1'
     AND kna1~sperr = @abap_false
     AND kna1~loevm = @abap_false
     AND kna1~nodel = @abap_false.

  DATA(t_cliente) = t_tax[].

  DELETE t_cliente WHERE partner = p_but000-partner. "p_kunnr.p_kunnr.

  READ TABLE t_cliente INTO DATA(w_cliente) INDEX 1.
*-CS2024000622-19.09.2024-JT-#152691-fim

  CHECK sy-subrc = 0.

  p_kunnr2 = w_cliente-partner.  "*-CS2024000622-19.09.2024-JT-#152691-fim
  v_stcd1  = w_cliente-taxnum.   "*-CS2024000622-19.09.2024-JT-#152691-fim

  CASE v_pergunta.
    WHEN abap_true.
* --->> cs1014779 - IR106423 ---->>
      CONCATENATE 'Código:' p_kunnr2 '-' v_stcd1 INTO v_text1.
*     CONCATENATE 'CNPJ:' v_stcd1 INTO v_text1.
      v_text2 = 'CNPJ DUPLICADO! Deseja criar um novo Parceiro de Negócio?'.

      IF sy-batch = abap_off.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            defaultoption = 'N'
            titel         = 'ATENÇÃO'
            textline1     = v_text1
            textline2     = v_text2
          IMPORTING
            answer        = v_res.
      ELSE.
        v_res = 'N'.
      ENDIF.

      IF v_res EQ 'N' OR v_res EQ 'A'.
        MESSAGE e008(zfi) RAISING error. "CNPJ Duplicado
      ELSE.
        PERFORM f_consistir_cnpj_insc_cli USING p_but000
                                                p_kunnr
                                                p_stcd1
                                                p_stcd3
                                                p_ktokd
                                       CHANGING e_flg
                                                e_kunnr.
      ENDIF.

    WHEN abap_false.
      MESSAGE e008(zfi) RAISING error. "CNPJ Duplicado
  ENDCASE.

ENDFORM.                    " f_consistir_cnpj

FORM f_consistir_cuit_cli  USING    p_but000 TYPE but000
                                    p_kunnr
                                    p_stcd1
                          CHANGING  p_mess
                                    p_kunnr2 LIKE kna1-kunnr.

  DATA: v_stcd1 TYPE kna1-stcd1.

  CHECK NOT p_stcd1 IS INITIAL.

*-CS2024000622-19.09.2024-JT-#152691-inicio
*  SELECT kunnr stcd1 INTO (p_kunnr2, v_stcd1)
*     FROM kna1 UP TO 1 ROWS
*     WHERE stcd1 = p_stcd1 AND
*           kunnr <> p_kunnr.
*  ENDSELECT.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
   INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd1
     AND taxtype    = 'AR1A' "// WBARBOSA 22/09/2025
     AND kna1~sperr = @abap_false
     AND kna1~loevm = @abap_false
     AND kna1~nodel = @abap_false.

  DATA(t_cliente) = t_tax[].

  DELETE t_cliente WHERE partner = p_but000-partner. "p_kunnr.p_kunnr.

  READ TABLE t_cliente INTO DATA(w_cliente) INDEX 1.

  CHECK sy-subrc = 0.

  p_kunnr2 = w_cliente-partner.
*-CS2024000622-19.09.2024-JT-#152691-fim

  IF ( sy-subrc EQ 0 ).
    IF ( p_mess EQ 'X' ).
      MESSAGE e057(zfi) WITH p_kunnr2 RAISING error. "CUIT Duplicado
    ELSE.
      p_mess = 'E'.
    ENDIF.
  ENDIF.

ENDFORM.                    " f_consistir_cnpj

FORM f_consistir_cpf_insc_cli  USING    p_but000 TYPE but000
                                        p_kunnr
                                        p_stcd2
                                        p_stcd3
                                        p_regio
                            CHANGING    p_mess
                                        p_kunnr2 LIKE kna1-kunnr.

*--> CS1007567/IR102937-->

  IF sy-tcode+3(1) NE '3'.
*<-- CS1007567/IR102937--<

    DATA: v_stcd2    TYPE kna1-stcd2,
          v_text1    TYPE spop-textline2,
          v_text2    TYPE spop-textline2,
          v_res      TYPE c LENGTH 1 VALUE 'N',
          v_pergunta TYPE char01.

    CHECK NOT p_stcd2 IS INITIAL.

    SELECT SINGLE valfrom
      FROM setleaf
      INTO @DATA(l_uf_perm_dupl)
     WHERE setname = 'MAGGI_BP_CPF_DUPL'
       AND valfrom = @p_regio .
    IF sy-subrc = 0.
      v_pergunta = abap_true.
    ENDIF.

*-CS2024000622-19.09.2024-JT-#152691-inicio
    "Seleciona apenas o que não está bloqueado para todas as empresas (SPERR) e o que não está marcado para eliminação (LOEVM).
*    SELECT kunnr stcd2 INTO (p_kunnr2, v_stcd2)
*       FROM kna1 UP TO 1 ROWS
*       WHERE stcd2 =  p_stcd2    AND
*             stcd3 =  p_stcd3    AND
*             kunnr <> p_kunnr    AND
*             sperr =  abap_false AND
*             loevm =  abap_false.
*    ENDSELECT.

    SELECT dfkkbptaxnum~*
      FROM dfkkbptaxnum
     INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
     INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
      INTO TABLE @DATA(t_tax)
     WHERE taxnum     = @p_stcd2
       AND taxtype    = 'BR2'
       AND kna1~sperr = @abap_false
       AND kna1~loevm = @abap_false
       AND kna1~nodel = @abap_false.

    SELECT dfkkbptaxnum~*
      FROM dfkkbptaxnum
     INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
     INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
 APPENDING TABLE @t_tax
     WHERE taxnum     = @p_stcd3
       AND taxtype    = 'BR3'
       AND kna1~sperr = @abap_false
       AND kna1~loevm = @abap_false
       AND kna1~nodel = @abap_false.

    DATA(t_cliente) = t_tax[].

    DELETE t_cliente WHERE partner = p_but000-partner. "p_kunnr.p_kunnr.
    DELETE ADJACENT DUPLICATES FROM t_cliente
                          COMPARING partner.

    LOOP AT t_cliente INTO DATA(w_cliente).
      READ TABLE t_tax INTO DATA(w_tax) WITH KEY partner = w_cliente-partner
                                                 taxtype = 'BR3'
                                                 taxnum  = p_stcd3.
      IF sy-subrc = 0.
        READ TABLE t_tax INTO w_tax WITH KEY partner = w_cliente-partner
                                             taxtype = 'BR2'
                                             taxnum  = p_stcd2.
        IF sy-subrc = 0.
          p_kunnr2 = w_tax-partner.
          v_stcd2 = w_tax-taxnum.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
*-CS2024000622-19.09.2024-JT-#152691-fim

*   IF ( sy-subrc EQ 0 ).
    IF v_stcd2 IS NOT INITIAL.  "*-CS2024000622-19.09.2024-JT-#152691
      CASE v_pergunta.
        WHEN abap_true.
* --->> ir110588 / cs1024167 ---->>
          CONCATENATE 'Código:'  p_kunnr2 '-'v_stcd2 INTO v_text1.
          v_text2 = 'CNPJ DUPLICADO - DESEJA GRAVAR?'.

          IF sy-batch = abap_off.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                defaultoption = 'N'
                titel         = 'ATENÇÃO'
                textline1     = v_text1
                textline2     = v_text2
              IMPORTING
                answer        = v_res.
          ELSE.
            v_res = 'N'.
          ENDIF.

          IF v_res EQ 'N' OR v_res EQ 'A'.
            MESSAGE e009(zfi) RAISING error. "CPF + Inscrição estadual duplicados !
          ENDIF.

        WHEN abap_false.
          MESSAGE e024(sd) WITH 'CPF + Inscrição estadual duplicados: ' p_kunnr2 '-' v_stcd2 RAISING error.
*         MESSAGE e009(zfi) RAISING error. "CPF + Inscrição estadual duplicados !
      ENDCASE.

    ENDIF.
  ENDIF.

ENDFORM.                    " f_consistir_cpf_insc

FORM f_consistir_cnpj_insc_cli  USING   p_but000 TYPE but000
                                        p_kunnr
                                        p_stcd1
                                        p_stcd3
                                        p_ktokd
                            CHANGING    p_mess
                                        p_kunnr2 LIKE kna1-kunnr.

  DATA: v_stcd1 TYPE kna1-stcd1,
        v_res   TYPE c LENGTH 1,
        v_res1  TYPE c LENGTH 1,
        v_text1 TYPE spop-textline2,
        v_text2 TYPE spop-textline2,
        v_erro  TYPE c LENGTH 1.

  CHECK NOT p_stcd1 IS INITIAL.

*-CS2024000622-19.09.2024-JT-#152691-inicio
  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
   INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd1
     AND taxtype    = 'BR1'
     AND kna1~sperr = @abap_false
     AND kna1~loevm = @abap_false
     AND kna1~nodel = @abap_false.

  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibupacustomer ON ibupacustomer~businesspartner = dfkkbptaxnum~partner
   INNER JOIN kna1          ON kna1~kunnr                    = ibupacustomer~customer
APPENDING TABLE @t_tax
   WHERE taxnum     = @p_stcd3
     AND taxtype    = 'BR3'
     AND kna1~sperr = @abap_false
     AND kna1~loevm = @abap_false
     AND kna1~nodel = @abap_false.

  DATA(t_cliente) = t_tax[].

  DELETE t_cliente WHERE partner = p_but000-partner. "p_kunnr.p_kunnr.
  DELETE ADJACENT DUPLICATES FROM t_cliente
                        COMPARING partner.

  LOOP AT t_cliente INTO DATA(w_cliente).
    READ TABLE t_tax INTO DATA(w_tax) WITH KEY partner = w_cliente-partner
                                               taxtype = 'BR3'
                                               taxnum  = p_stcd3.
    IF sy-subrc = 0.
      READ TABLE t_tax INTO w_tax WITH KEY partner = w_cliente-partner
                                           taxtype = 'BR1'
                                           taxnum  = p_stcd1.
      IF sy-subrc = 0.
        p_kunnr2 = w_tax-partner.
        v_stcd1 = w_tax-taxnum.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF v_stcd1 IS NOT INITIAL.  "*-CS2024000622-19.09.2024-JT-#152691
    MESSAGE e010(zfi) RAISING error. "CNPJ + Inscrição estadual duplicados !
  ENDIF.
*-CS2024000622-19.09.2024-JT-#152691-fim

*  IF p_ktokd EQ 'ZCPJ'.
*    "Seleciona apenas o que não está bloqueado para todas as empresas (SPERR) e o que não está marcado para eliminação (LOEVM).
*    SELECT kunnr stcd1 INTO (p_kunnr2, v_stcd1)
*      FROM kna1 UP TO 1 ROWS
*      WHERE stcd1 =  p_stcd1    AND
*            stcd3 =  p_stcd3    AND
*            kunnr <> p_kunnr    AND
*            sperr EQ abap_false AND
*            loevm =  abap_false.
*      v_erro = 1.
*    ENDSELECT.
*
*    IF sy-subrc NE 0.
*      v_erro = 2.
*      SELECT kunnr stcd1 INTO (p_kunnr2, v_stcd1)
*     FROM kna1 UP TO 1 ROWS
*     WHERE stcd1 =  p_stcd1    AND
*           kunnr <> p_kunnr    AND
*           sperr EQ abap_false AND
*           loevm =  abap_false.
*        v_erro = 2.
*      ENDSELECT.
*    ENDIF.
*  ELSE.
*    SELECT kunnr stcd1 INTO (p_kunnr2, v_stcd1)
*      FROM kna1 UP TO 1 ROWS
*      WHERE stcd1 = p_stcd1 AND
*            stcd3 = p_stcd3 AND
*            kunnr <> p_kunnr.
*      v_erro = 1.
*    ENDSELECT.
*
*    IF sy-subrc NE 0.
*      v_erro = 2.
*      SELECT kunnr stcd1 INTO (p_kunnr2, v_stcd1)
*      FROM kna1 UP TO 1 ROWS
*      WHERE stcd1 =  p_stcd1    AND
*            kunnr <> p_kunnr    AND
*            sperr EQ abap_false AND
*            loevm =  abap_false.
*        v_erro = 2.
*      ENDSELECT.
*    ENDIF.
*  ENDIF.
*
*  IF ( sy-subrc EQ 0 ).
*    MESSAGE e010(zfi) RAISING error. "CNPJ + Inscrição estadual duplicados !
*  ENDIF.

*    IF ( p_mess EQ 'X' ).
*
*      CONCATENATE 'Código: ' p_kunnr2 '-' v_stcd1 INTO v_text1.
*      IF v_erro EQ 1.
*        v_text2 = 'CNPJ + INSC.ESTADUAL DUPLICADO - Deseja continuar?'.
*      ELSEIF v_erro EQ 2.
*        v_text2 = 'CNPJ DUPLICADO - Deseja continuar?'.
*      ENDIF.
*
*      IF sy-batch = abap_off.
*        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*          EXPORTING
*            defaultoption = 'N'
*            titel         = 'ATENÇÃO'
*            textline1     = v_text1
*            textline2     = v_text2
*          IMPORTING
*            answer        = v_res.
*      ELSE.
*        v_res = 'N'.
*      ENDIF.
*
*    ELSE.
*      p_mess = 'E'.
*    ENDIF.
*  ENDIF.
*
*  IF v_res EQ 'N' OR v_res EQ 'A'.
*    MESSAGE e010(zfi) RAISING error. "CNPJ + Inscrição estadual duplicados !
*  ENDIF.

ENDFORM.                    " f_consistir_CNPJ_insc

*-#155327-16.10.2024-JT-inicio
FORM f_check_validar_dados_cliente USING p_but000   TYPE but000
                                         p_kna1     TYPE kna1
                                         p_knb1     TYPE knb1
                                         p_knvv     TYPE knvv
                                CHANGING p_validar.

  DATA: lc_base_but000 TYPE but000,
        lc_base_kna1   TYPE kna1,
        lc_base_knb1   TYPE knb1,
        lc_base_knvv   TYPE knvv.

  p_validar = abap_false.

  IF p_kna1-kunnr(2) = '##'.   "Inclusao parceiro
    p_validar = abap_true.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM but000
    INTO lc_base_but000
   WHERE partner = p_but000-partner.

  IF p_kna1 IS NOT INITIAL.
    SELECT SINGLE *
      FROM kna1
      INTO lc_base_kna1
     WHERE kunnr = p_kna1-kunnr.
  ENDIF.

  IF p_knb1 IS NOT INITIAL.
    SELECT SINGLE *
      FROM knb1
      INTO lc_base_knb1
     WHERE kunnr = p_knb1-kunnr
       AND bukrs = p_knb1-bukrs.
  ENDIF.

  IF p_knvv IS NOT INITIAL.
    SELECT SINGLE *
      FROM knvv
      INTO lc_base_knvv
     WHERE kunnr = p_knvv-kunnr
       AND vkorg = p_knvv-vkorg
       AND vtweg = p_knvv-vtweg
       AND spart = p_knvv-spart.
  ENDIF.

  "FLCU01 - Função cliente Venda
  "Aba = Cliente: dados gerais
  IF p_kna1-nodel          = lc_base_kna1-nodel          AND
     p_kna1-loevm          = lc_base_kna1-loevm          AND
  "Aba = Cliente: dados gerais
     p_kna1-cassd          = lc_base_kna1-cassd          AND
     p_knvv-cassd          = lc_base_knvv-cassd          AND
     p_kna1-aufsd          = lc_base_kna1-aufsd          AND
     p_knvv-aufsd          = lc_base_knvv-aufsd          AND
     p_kna1-lifsd          = lc_base_kna1-lifsd          AND
     p_knvv-lifsd          = lc_base_knvv-lifsd          AND
     p_kna1-faksd          = lc_base_kna1-faksd          AND
     p_knvv-faksd          = lc_base_knvv-faksd          AND
     p_knvv-loevm          = lc_base_knvv-loevm          AND
  "FLCU00 - Função Cliente empresa
  "Aba = Cliente: dados gerais
     p_kna1-nodel          = lc_base_kna1-nodel          AND
     p_kna1-loevm          = lc_base_kna1-loevm          AND
  "Aba = Cliente: Status  ( por empresa)
     p_kna1-sperr          = lc_base_kna1-sperr          AND
     p_knb1-sperr          = lc_base_knb1-sperr          AND
     p_knb1-nodel          = lc_base_knb1-nodel          AND
     p_knb1-loevm          = lc_base_knb1-loevm          AND
  "000000 - Parceiro negócios Geral
  "Aba = Status
     p_but000-xblck        = lc_base_but000-xblck        AND
     p_but000-not_released = lc_base_but000-not_released AND
     p_but000-xdele        = lc_base_but000-xdele.
    p_validar = abap_true.
  ELSE.
    p_validar = abap_false.
  ENDIF.

ENDFORM.

FORM f_check_validar_dados_fornece USING p_but000   TYPE but000
                                         p_lfa1     TYPE lfa1
                                         p_lfm1     TYPE lfm1  "*-#156755-31.10.2024-JT-inicio
                                         p_lfb1     TYPE lfb1
                                CHANGING p_validar.

  DATA: lc_base_but000 TYPE but000,
        lc_base_lfa1   TYPE lfa1,
        lc_base_lfm1   TYPE lfm1,
        lc_base_lfb1   TYPE lfb1.

  p_validar = abap_false.

  IF p_lfa1-lifnr(2) = '##'.   "Inclusao parceiro
    p_validar = abap_true.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM but000
    INTO lc_base_but000
   WHERE partner = p_but000-partner.

  IF p_lfa1 IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfa1
      INTO lc_base_lfa1
     WHERE lifnr = p_lfa1-lifnr.
  ENDIF.

*-#156755-31.10.2024-JT-inicio
  IF p_lfm1 IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfm1
      INTO lc_base_lfm1
     WHERE lifnr = p_lfm1-lifnr
       AND ekorg = p_lfm1-ekorg.
  ENDIF.
*-#156755-31.10.2024-JT-fim

  IF p_lfb1 IS NOT INITIAL.
    SELECT SINGLE *
      FROM lfb1
      INTO lc_base_lfb1
     WHERE lifnr = p_lfb1-lifnr
       AND bukrs = p_lfb1-bukrs.
  ENDIF.

  "FLVN01 - Função Fornecedor compra
  "Aba = Fornecedor: dados gerais
  IF p_lfa1-sperq          = lc_base_lfa1-sperq          AND
     p_lfa1-nodel          = lc_base_lfa1-nodel          AND
     p_lfa1-loevm          = lc_base_lfa1-loevm          AND
     p_lfa1-sperz          = lc_base_lfa1-sperz          AND
    "Aba: Dados de compra  ( por Organização de compra)
     p_lfa1-sperm          = lc_base_lfa1-sperm          AND
     p_lfm1-sperm          = lc_base_lfm1-sperm          AND
  "FLVN00 - Função Fornecedor empresa
  "Aba = Fornecedor: dados gerais
     p_lfa1-nodel          = lc_base_lfa1-nodel          AND
     p_lfa1-loevm          = lc_base_lfa1-loevm          AND
     p_lfa1-sperz          = lc_base_lfa1-sperz          AND
     "Aba = Fornecedor: Status  ( por empresa)
     p_lfa1-sperr          = lc_base_lfa1-sperr          AND
     p_lfb1-sperr          = lc_base_lfb1-sperr          AND
     p_lfb1-nodel          = lc_base_lfb1-nodel          AND
     p_lfb1-loevm          = lc_base_lfb1-loevm          AND
  "000000 - Parceiro negócios Geral
  "Aba = Status
     p_but000-xblck        = lc_base_but000-xblck        AND
     p_but000-not_released = lc_base_but000-not_released AND
     p_but000-xdele        = lc_base_but000-xdele.
    p_validar = abap_true.
  ELSE.
    p_validar = abap_false.
  ENDIF.

ENDFORM.
*-#155327-16.10.2024-JT-fim
