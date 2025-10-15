*&---------------------------------------------------------------------*
*& Include ZFIR0100_DADOS
*&---------------------------------------------------------------------*

FORM pega_dados .

  SELECT *
  FROM zsdt0306
  WHERE 1 = 1
  AND id_seq        = @s_lote
  AND bukrs         = @s_bukrs
  AND werks         = @s_werks
  INTO TABLE @DATA(it_zsdt0306).

  IF it_zsdt0306 IS NOT INITIAL.
    FREE: t_saida1[],t_zsdt0306.
    CLEAR: w_saida1.
    MOVE-CORRESPONDING it_zsdt0306 TO t_zsdt0306.
    FREE:it_zsdt0306.
    LOOP AT t_zsdt0306 ASSIGNING FIELD-SYMBOL(<_define_alv1>).

      w_saida1-id_seq = <_define_alv1>-id_seq.
      w_saida1-bukrs = <_define_alv1>-bukrs.
      w_saida1-werks = <_define_alv1>-werks.
      w_saida1-bukrs_fat = <_define_alv1>-bukrs_fat.
      w_saida1-werks_fat = <_define_alv1>-werks_fat.
      w_saida1-ebeln = <_define_alv1>-ebeln.
      w_saida1-ebelp = <_define_alv1>-ebelp.
      w_saida1-matnr = <_define_alv1>-matnr.
      w_saida1-operacao = <_define_alv1>-operacao.
      w_saida1-qtdpedid = <_define_alv1>-quantidade.
      w_saida1-safra    = <_define_alv1>-unsez.      "*-IR194132-29.10.2024-#156085-JT

      SELECT SINGLE matkl FROM mara WHERE matnr = @<_define_alv1>-matnr INTO @w_saida1-matkl.
      SELECT SINGLE maktx FROM makt WHERE matnr = @<_define_alv1>-matnr AND spras = @sy-langu INTO @w_saida1-maktx.

      APPEND w_saida1 TO t_saida1.
      CLEAR: w_saida1.
    ENDLOOP.

    SELECT * FROM zsdt0306_fat
      FOR ALL ENTRIES IN @t_zsdt0306
      WHERE id_seq     = @t_zsdt0306-id_seq
      INTO TABLE @DATA(t_zsdt0306_fat).

    MOVE-CORRESPONDING t_zsdt0306_fat TO t_saida2[].

  ENDIF.

ENDFORM.
