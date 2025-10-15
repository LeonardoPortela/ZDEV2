FUNCTION z_memo_atualiza_re.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(PZDDE_APLICACAO) TYPE  ZDDE_APLICACAO
*"----------------------------------------------------------------------

  DATA: wa_zreg_exportacao TYPE zreg_exportacao,
        wa_zdoc_exp        TYPE zdoc_exp,
        wa_j_1bnflin       TYPE j_1bnflin,
        it_zdoc_exp        TYPE TABLE OF zdoc_exp INITIAL SIZE 0 WITH HEADER LINE,
        wa_vbrp            TYPE vbrp,
        vg_refkey          TYPE j_1brefkey,
        wa_memo_nota       TYPE zdoc_memo_nf_exp,
        it_memorando       TYPE TABLE OF zdoc_memorando INITIAL SIZE 0 WITH HEADER LINE,
        wa_memorando       TYPE zdoc_memorando,
        wa_zdde_aplicacao  TYPE zdde_aplicacao,
        wa_zdde            TYPE zdde.

  CHECK NOT pzdde_aplicacao IS INITIAL.

  SELECT SINGLE * INTO wa_zreg_exportacao
    FROM zreg_exportacao
   WHERE id_registro_expo EQ pzdde_aplicacao-id_registro_expo.

  "Achou RE
  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO wa_zdde_aplicacao
    FROM zdde_aplicacao
   WHERE id_registro_expo EQ wa_zreg_exportacao-id_registro_expo.

  "Achou RE aplicada em DDE
  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO wa_zdde
    FROM zdde
   WHERE id_dde EQ wa_zdde_aplicacao-id_dde.

  "Achou DDE da aplicação
  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE it_zdoc_exp
    FROM zdoc_exp
   WHERE id_registro_expo EQ pzdde_aplicacao-id_registro_expo.

  LOOP AT it_zdoc_exp INTO wa_zdoc_exp.

    wa_zdoc_exp-id_dde = wa_zdde-id_dde.
    wa_zdoc_exp-nr_dde = wa_zdde-nr_dde.
    MODIFY zdoc_exp FROM wa_zdoc_exp.

    SELECT SINGLE * INTO wa_vbrp
      FROM vbrp
     WHERE vgbel EQ wa_zdoc_exp-vbeln AND DRAFT = SPACE .

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    vg_refkey = wa_vbrp-vbeln.

    SELECT SINGLE * INTO wa_j_1bnflin
      FROM j_1bnflin
     WHERE reftyp EQ 'BI'
       AND refkey EQ vg_refkey.

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      INTO wa_memo_nota
      FROM zdoc_memo_nf_exp
     WHERE docnum EQ wa_j_1bnflin-docnum.

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT * INTO TABLE it_memorando
      FROM zdoc_memorando
     WHERE nr_nota_exp EQ wa_memo_nota-nr_nota_exp
       AND ( status EQ space OR status EQ 'P').

    IF NOT sy-subrc IS INITIAL.
      CONTINUE.
    ENDIF.

    LOOP AT it_memorando INTO wa_memorando.

      CALL FUNCTION 'CONVERSION_EXIT_ZDDEX_INPUT'
        EXPORTING
          input  = wa_zdde-nr_dde
        IMPORTING
          output = wa_memorando-nr_dde.

      wa_memorando-dt_dde = wa_zdde-dt_dde.
      MODIFY zdoc_memorando FROM wa_memorando.

    ENDLOOP.

  ENDLOOP.

ENDFUNCTION.
