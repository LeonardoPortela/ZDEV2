*&---------------------------------------------------------------------*
*& Report  ZMMR0040
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr0040.

TABLES rkpf.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_data  FOR rkpf-rsdat.
SELECTION-SCREEN: END OF BLOCK b1.

DATA: it_saida TYPE TABLE OF zmme0003.
DATA: wa_saida TYPE zmme0003.

START-OF-SELECTION.

  SELECT rsnum
    FROM zmmt0087 AS a
    JOIN mkpf AS b ON b~mblnr EQ a~mblnr
    INTO TABLE @DATA(it_rsnum)
    WHERE b~budat IN @p_data.

  SELECT *
    FROM rkpf AS a
    INTO TABLE @DATA(it_rkpf)
    FOR ALL ENTRIES IN @it_rsnum
    WHERE rsnum EQ @it_rsnum-rsnum
      AND aufnr <> ' '.

  CHECK it_rkpf IS NOT INITIAL.

  SELECT *
    FROM resb
    INTO TABLE @DATA(it_resb)
        FOR ALL ENTRIES IN @it_rkpf
    WHERE rsnum EQ @it_rkpf-rsnum.

  IF it_resb IS NOT INITIAL.

    SELECT matnr, maktx
      FROM makt
      INTO TABLE @DATA(it_makt)
          FOR ALL ENTRIES IN @it_resb
      WHERE matnr EQ @it_resb-matnr.

    SELECT rsnum, rspos, mblnr, operacao, count_devolucao, quant, lgort, pernr, lado_log, data, hora, operador
      FROM zmmt0087
      INTO TABLE @DATA(it_zmmt0087)
          FOR ALL ENTRIES IN @it_resb
      WHERE rsnum EQ @it_resb-rsnum
      AND rspos EQ @it_resb-rspos.

    IF it_zmmt0087 IS NOT INITIAL.

      SELECT pernr, cname
        FROM pa0002
        INTO TABLE @DATA(it_pa0002)
            FOR ALL ENTRIES IN @it_zmmt0087
        WHERE pernr EQ @it_zmmt0087-pernr.

      SELECT *
        FROM mkpf
        INTO TABLE @DATA(it_mkpf)
            FOR ALL ENTRIES IN @it_zmmt0087
        WHERE mblnr EQ @it_zmmt0087-mblnr
          AND budat IN @p_data.

    ENDIF.

  ENDIF.


  LOOP AT it_mkpf INTO DATA(wa_mkpf).

    wa_saida-mblnr = wa_mkpf-mblnr.
    wa_saida-bktxt = wa_mkpf-bktxt.

    READ TABLE it_zmmt0087 INTO DATA(wa_zmmt0087) WITH KEY mblnr = wa_mkpf-mblnr.
    wa_saida-data = wa_zmmt0087-data.
    wa_saida-hora = wa_zmmt0087-hora.
    wa_saida-operador = wa_zmmt0087-operador.

    READ TABLE it_resb INTO DATA(wa_resb) WITH KEY rsnum = wa_zmmt0087-rsnum rspos = wa_zmmt0087-rspos.

    wa_saida-bdmng = wa_resb-bdmng.
    wa_saida-meins = wa_resb-meins.
    wa_saida-erfmg = wa_resb-erfmg.
    wa_saida-erfme = wa_resb-erfme.
    wa_saida-enwrt = wa_resb-enwrt.
    wa_saida-waers = wa_resb-waers.
    wa_saida-rssta = wa_resb-rssta.

    READ TABLE it_rkpf INTO DATA(wa_rkpf) WITH KEY rsnum = wa_resb-rsnum.
    wa_saida-rsnum = wa_rkpf-rsnum.
    wa_saida-usnam = wa_rkpf-usnam.
    wa_saida-pargb = wa_rkpf-pargb.
    wa_saida-bwart = wa_rkpf-bwart.
    wa_saida-umlgo = wa_rkpf-umlgo.
    wa_saida-rsdat = wa_rkpf-rsdat.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_resb-matnr.
    wa_saida-matnr = wa_makt-matnr.
    wa_saida-maktx = wa_makt-maktx.

    READ TABLE it_pa0002 INTO DATA(wa_pa0002) WITH KEY pernr = wa_zmmt0087-pernr.
    wa_saida-pernr = wa_pa0002-pernr.
    wa_saida-cname = wa_pa0002-cname.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_mkpf, wa_pa0002, wa_zmmt0087, wa_makt, wa_rkpf, wa_resb.

  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'ZMME0003'
    TABLES
      t_outtab         = it_saida
    EXCEPTIONS
      program_error    = 1
      OTHERS           = 2.
