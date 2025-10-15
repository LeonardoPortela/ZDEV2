*----------------------------------------------------------------------*
***INCLUDE ZXF05F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTIR_CPF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_consistir_cpf USING p_but000 TYPE but000
                           p_lifnr p_stcd2 p_ktokk p_sperr p_loevm p_nodel.

  DATA: v_lifnr TYPE lfa1-lifnr,
        v_stcd2 TYPE lfa1-stcd2,
        v_sperr TYPE lfa1-sperr,
        v_text1 TYPE spop-textline2,
        v_text2 TYPE spop-textline2,
        v_res   TYPE c LENGTH 1 VALUE 'N'.

  DATA: it_ktokk TYPE zrange_ktokk_t,
        wa_ktokk TYPE zrange_ktokk.

  CHECK NOT p_stcd2 IS INITIAL.

  DATA(lc_sair) = abap_false.

*-CS2024000622-19.09.2024-JT-#152691-inicio
*  IF p_lifnr IS NOT INITIAL.
  SELECT SINGLE * INTO @DATA(wa_lfa1)
    FROM lfa1
   WHERE lifnr EQ @p_lifnr.
*
*    IF sy-subrc IS INITIAL.
*      IF wa_lfa1-sperr EQ abap_false AND p_sperr EQ abap_true.
*        lc_sair = abap_true.
*      ENDIF.
*
*      IF wa_lfa1-loevm EQ abap_false AND p_loevm EQ abap_true.
*        lc_sair = abap_true.
*      ENDIF.
*
*      IF wa_lfa1-nodel EQ abap_false AND p_nodel EQ abap_true.
*        lc_sair = abap_true.
*      ENDIF.
*    ENDIF.
*  ENDIF.
  "Usuário não está bloqueando um cadastro não precisa bloquear
* CHECK lc_sair EQ abap_false.
*-CS2024000622-19.09.2024-JT-#152691-fim

  SELECT SINGLE *
    FROM setleaf INTO @DATA(_wl_setleaf_xk99)
   WHERE setname EQ 'MAGGI_XK99_USER'
     AND valfrom EQ @sy-uname.

  "Caso usuário tenha perfil de acesso deve avisar que será "feito uma coisa errada".
  DATA(lc_acesso_pode_fazer_tudo) = COND string( WHEN sy-subrc IS INITIAL THEN abap_true ELSE abap_false ) .

*-CS2024000622-19.09.2024-JT-#152691-incio
  SELECT dfkkbptaxnum~*
    FROM dfkkbptaxnum
   INNER JOIN ibpsupplier   ON ibpsupplier~businesspartner = dfkkbptaxnum~partner
   INNER JOIN lfa1          ON lfa1~lifnr                  = ibpsupplier~supplier
    INTO TABLE @DATA(t_tax)
   WHERE taxnum     = @p_stcd2
     AND taxtype    = 'BR2'
     AND ktokk      = @p_ktokk    "*-CS2024000622-19.09.2024-JT-#152691-fim
     AND lfa1~sperr = @abap_false
     AND lfa1~loevm = @abap_false
     AND lfa1~nodel = @abap_false.

  DATA(t_fornece) = t_tax[].

  DELETE t_fornece WHERE partner = p_but000-partner.

  READ TABLE t_fornece INTO DATA(w_fornece) INDEX 1.

  CHECK sy-subrc = 0.

  wa_lfa1-lifnr = w_fornece-partner.  "*-CS2024000622-19.09.2024-JT-#152691-fim

  CASE lc_acesso_pode_fazer_tudo.
    WHEN abap_true.
      MESSAGE w073(zfi) WITH wa_lfa1-lifnr RAISING error. "072 Deve ser Marcado p/ Eliminação o fornecedor &1!
    WHEN abap_false.
      MESSAGE e073(zfi) WITH wa_lfa1-lifnr RAISING error. "072 Deve ser Marcado p/ Eliminação o fornecedor &1!
  ENDCASE.
*-CS2024000622-19.09.2024-JT-#152691-fim

*-CS2024000622-19.09.2024-JT-#152691-incio
*  CLEAR: it_ktokk.
*
*  CASE p_ktokk.
*    WHEN 'ZMOT'.
*      wa_ktokk-sign   = 'I'.
*      wa_ktokk-option = 'EQ'.
*      wa_ktokk-low    = p_ktokk.
*      wa_ktokk-high   = p_ktokk.
*      APPEND wa_ktokk TO it_ktokk.
*
*      "Busca Todos os Cadastro do CPF menos o Atual.
*      SELECT * INTO TABLE @DATA(it_lfa1)
*        FROM lfa1
*       WHERE stcd2 EQ @p_stcd2
*         AND lifnr NE @p_lifnr
*         AND ktokk IN @it_ktokk.
*
*      CHECK sy-subrc IS INITIAL.
*
*      LOOP AT it_lfa1 INTO wa_lfa1.
*
*        "Deve ser Bloqueado para contabilização o fornecedor
*        "Deve ser Marcado p/ Eliminação o fornecedor
*        IF wa_lfa1-sperr EQ abap_false OR wa_lfa1-loevm EQ abap_false.
*
*          "Deve ser Bloqueado para contabilização o fornecedor
*          IF wa_lfa1-sperr EQ abap_false.
*            CASE lc_acesso_pode_fazer_tudo.
*              WHEN abap_true.
*                MESSAGE w024(sd) WITH 'Ja existem esses dados fiscais no fornecedor' wa_lfa1-lifnr RAISING error.
**               MESSAGE w072(zfi) WITH wa_lfa1-lifnr RAISING error. "072 Deve ser Bloqueado para contabilização o fornecedor &1!
*              WHEN abap_false.
*                MESSAGE e024(sd) WITH 'Ja existem esses dados fiscais no fornecedor' wa_lfa1-lifnr RAISING error.
**               MESSAGE e072(zfi) WITH wa_lfa1-lifnr RAISING error.  "072 Deve ser Bloqueado para contabilização o fornecedor &1!
*            ENDCASE.
*          ENDIF.
*
*          "Deve ser Marcado p/ Eliminação o fornecedor
*          IF wa_lfa1-loevm EQ abap_false.
*            CASE lc_acesso_pode_fazer_tudo.
*              WHEN abap_true.
*                MESSAGE w073(zfi) WITH wa_lfa1-lifnr RAISING error. "072 Deve ser Marcado p/ Eliminação o fornecedor &1!
*              WHEN abap_false.
*                MESSAGE e073(zfi) WITH wa_lfa1-lifnr RAISING error. "072 Deve ser Marcado p/ Eliminação o fornecedor &1!
*            ENDCASE.
*          ENDIF.
*
*        ENDIF.
*      ENDLOOP.
*
*      "CPF possui cadastro, favor ajustar um cadastro existente!
*      "Bloqueio para não criar mais cadastro de fonecedor
*      IF p_lifnr IS INITIAL AND it_lfa1[] IS NOT INITIAL.
*        CASE lc_acesso_pode_fazer_tudo.
*          WHEN abap_true.
*            MESSAGE w074(zfi) RAISING error. "074 CPF possui cadastro, favor ajustar um cadastro existente!
*          WHEN abap_false.
*            MESSAGE e074(zfi) RAISING error. "074 CPF possui cadastro, favor ajustar um cadastro existente!
*        ENDCASE.
*      ENDIF.
*
*    WHEN OTHERS.
*      wa_ktokk-sign   = 'I'.
*      wa_ktokk-option = 'NE'.
*      wa_ktokk-low    = 'ZMOT'.
*      wa_ktokk-high   = 'ZMOT'.
*      APPEND wa_ktokk TO it_ktokk.
*
*      SELECT lifnr stcd2 sperr INTO (v_lifnr, v_stcd2,v_sperr)
*        FROM lfa1 UP TO 1 ROWS
*       WHERE stcd2 EQ p_stcd2
*         AND lifnr NE p_lifnr
*         AND ktokk IN it_ktokk
*         AND sperr NE 'X'
*         AND loevm NE 'X'.
*      ENDSELECT.
*
*      IF sy-subrc IS INITIAL.
*        CASE lc_acesso_pode_fazer_tudo.
*          WHEN abap_true.
*            MESSAGE w007(zfi) RAISING error. "CPF Duplicado
*          WHEN abap_false.
*            MESSAGE e007(zfi) RAISING error. "CPF Duplicado  "*-CS2024000622-26.07.2024-JT-#146685
*
**-CS2024000622-26.07.2024-JT-#146685-inicio
** ---> CS1024167 / IR110588 --->
**            CONCATENATE 'Código:'  v_lifnr '-' p_stcd2 INTO v_text1.
***           CONCATENATE 'CPF:' p_stcd2 INTO v_text1.
**            v_text2 = 'CPF DUPLICADO - Deseja Realmente Gravar?'.
**
**            IF sy-batch = abap_off.
**              CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
**                EXPORTING
**                  defaultoption = 'N'
**                  titel         = 'ATENÇÃO'
**                  textline1     = v_text1
**                  textline2     = v_text2
**                IMPORTING
**                  answer        = v_res.
**            ELSE.
**              v_res = 'N'.
**            ENDIF.
**
**            IF v_res EQ 'N' OR v_res EQ 'A'.
**              MESSAGE e007(zfi) RAISING error. "CPF Duplicado
**            ENDIF.
** <--- CS1024167 / IR110588 ---
**-CS2024000622-26.07.2024-JT-#146685-fim
*        ENDCASE.
*      ENDIF.
*
*  ENDCASE.
*-CS2024000622-19.09.2024-JT-#152691-fim

ENDFORM.                    " F_CONSISTIR_CPF
*&---------------------------------------------------------------------*
*&      Form  F_CONSISTIR_CPF_GRUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LFA1_LIFNR  text
*      -->P_I_LFA1_STCD2  text
*      -->P_I_LFA1_KTOKK  text
*----------------------------------------------------------------------*
FORM f_consistir_cpf_grupo  USING    p_lifnr p_stcd2 p_ktokk.

  DATA: v_lifnr TYPE lfa1-lifnr,
        v_stcd2 TYPE lfa1-stcd2,
        v_text1 TYPE spop-textline2,
        v_text2 TYPE spop-textline2,
        v_res   TYPE c LENGTH 1 VALUE 'N'.

  CHECK NOT p_stcd2 IS INITIAL.

  SELECT lifnr stcd2 INTO (v_lifnr, v_stcd2)
     FROM lfa1 UP TO 1 ROWS
     WHERE stcd2 = p_stcd2 AND
           ktokk =  p_ktokk AND
           lifnr <> p_lifnr.
  ENDSELECT.

  IF sy-subrc = 0.
* ---> CS1024167 / IR110588 --->
    CONCATENATE 'Código:'  v_lifnr '-' v_stcd2 INTO v_text1.
*   CONCATENATE 'CPF:' v_stcd2 INTO v_text1.
    v_text2 = 'CPF DUPLICADO - Deseja Realmente Gravar?'.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        defaultoption = 'N'
        titel         = 'ATENÇÃO'
        textline1     = v_text1
        textline2     = v_text2
      IMPORTING
        answer        = v_res.

    IF v_res EQ 'N' OR v_res EQ 'A'.
      MESSAGE e007(zfi). "CPF Duplicado
    ENDIF.

* <--- CS1024167 / IR110588 ---
  ENDIF.

ENDFORM.                    " F_CONSISTIR_CPF_GRUPO
