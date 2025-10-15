FORM f_zpmr0007_valida_usuario.

  FIELD-SYMBOLS <fs_entry> TYPE any.
  DATA: lv_line   TYPE string,
        lv_mandt  TYPE mandt,
        lv_bukrs  TYPE bukrs,
        lv_werks  TYPE werks_d,
        lv_usnam  TYPE usnam,
        lv_action,
        lv_exists TYPE abap_bool.

  LOOP AT total ASSIGNING FIELD-SYMBOL(<lv_line>).

    lv_mandt = <lv_line>+0(3).
    lv_bukrs = <lv_line>+3(4).
    lv_werks = <lv_line>+7(4).
    lv_usnam = <lv_line>+11(12).
    lv_action = <lv_line>+23(1).

    CHECK lv_action = 'N' OR
          lv_action = 'U'.

    TRANSLATE lv_usnam TO UPPER CASE.

    CHECK lv_usnam IS NOT INITIAL.

    "=== Verifica se o usuário já é aprovador em outra tabela ===
    SELECT SINGLE aprovador
      FROM zpmr0002
      WHERE aprovador  = @lv_usnam
         OR usua_subst = @lv_usnam
      INTO @DATA(lv_aprovador_0002).

    IF sy-subrc = 0.
      MESSAGE |Usuário { lv_usnam } já está cadastrado como aprovador na transação ZPM0021.| TYPE 'E'.
    ENDIF.

    SELECT SINGLE aprovador
      FROM zpmr0011
      WHERE aprovador  = @lv_usnam
         OR usua_subst = @lv_usnam
      INTO @DATA(lv_aprovador_0011).

    IF sy-subrc = 0.
      MESSAGE |Usuário { lv_usnam } já está cadastrado como aprovador na transação ZPM0021.| TYPE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.
