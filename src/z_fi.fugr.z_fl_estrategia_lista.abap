FUNCTION z_fl_estrategia_lista.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"     VALUE(V_LOTE) LIKE  ZHCMT_PY_0004-LOTE OPTIONAL
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZFI_LOTES_FOL
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_FOL OPTIONAL
*"      T_DOCS STRUCTURE  ZSFI_APROV_PAGTOSAL_DOC OPTIONAL
*"----------------------------------------------------------------------
*
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:
    BEGIN OF ty_t001,
      bukrs TYPE t001-bukrs,
      butxt TYPE t001-butxt,
      land1 TYPE t001-land1,
      waers TYPE t001-waers,
    END OF ty_t001.


  DATA: tg_lotes           TYPE TABLE OF zfi_lotes_fol WITH HEADER LINE,
        it_zhcmt_py_0004   TYPE TABLE OF zhcmt_py_0004,
        it_zhcmt_py_0004_t TYPE TABLE OF zhcmt_py_0004,
        it_zhcmt_py_0005   TYPE TABLE OF zhcmt_py_0005,
        it_zhcmt_py_0006   TYPE TABLE OF zhcmt_py_0006,

        it_t001            TYPE TABLE OF ty_t001,
        it_estra           TYPE TABLE OF zfi_estrategia_fol.

  DATA: wa_zhcmt_py_0004   TYPE zhcmt_py_0004,
        wa_zhcmt_py_0004_t TYPE zhcmt_py_0004,
        wa_zhcmt_py_0005   TYPE zhcmt_py_0005,
        wa_zhcmt_py_0006   TYPE zhcmt_py_0006,

        wa_t001            TYPE ty_t001,
        wa_estra           TYPE zfi_estrategia_fol.

  DATA: lr_lote TYPE RANGE OF zlote_num.

  DATA: vflg_ico(1),
        vflag(1),
        xtotal       TYPE zhcmt_py_0004-betrg.
**********************************************************************************************************************

  IF v_lote IS NOT INITIAL.
    SELECT *
     FROM zhcmt_py_0004
     INTO TABLE it_zhcmt_py_0004
    WHERE lote = v_lote.
  ELSE.
    SELECT *
      FROM zhcmt_py_0004
      INTO TABLE it_zhcmt_py_0004
      WHERE status EQ 'LIB'
      AND    ocrsn IN ( 'EMPR', 'RESC', 'FERI','DIFE','RECO' , 'PPRP' ).

    SELECT *
      FROM zhcmt_py_0004
    APPENDING TABLE it_zhcmt_py_0004
      WHERE status EQ 'LIB'
      AND tp_pgto EQ 'FOLHAPGTO'.
  ENDIF.

  CHECK it_zhcmt_py_0004[] IS NOT INITIAL.

  it_zhcmt_py_0004_t[] = it_zhcmt_py_0004[].
  SORT it_zhcmt_py_0004   BY lote.
  SORT it_zhcmt_py_0004_t BY lote.
  DELETE ADJACENT DUPLICATES FROM it_zhcmt_py_0004_t COMPARING lote.

  SELECT *
    FROM zhcmt_py_0005
    INTO TABLE it_zhcmt_py_0005
     FOR ALL ENTRIES IN it_zhcmt_py_0004
    WHERE bukrs      LE it_zhcmt_py_0004-bukrs
    AND   bukrs_ate  GE it_zhcmt_py_0004-bukrs
    AND   abkrs      EQ it_zhcmt_py_0004-abkrs
*    AND   DT_VAL_DE  LE SY-DATUM
    AND   dt_val_ate >= sy-datum. "GE SY-DATUM.

  SORT it_zhcmt_py_0005   BY bukrs bukrs_ate abkrs nivel.

  SELECT  *
  FROM zhcmt_py_0006
  INTO TABLE it_zhcmt_py_0006
  FOR ALL ENTRIES IN it_zhcmt_py_0004
  WHERE lote EQ it_zhcmt_py_0004-lote.

  DELETE it_zhcmt_py_0006[] WHERE nivel EQ 'X'.

  SELECT bukrs butxt land1 waers
  FROM t001
  INTO TABLE it_t001
  FOR ALL ENTRIES IN it_zhcmt_py_0004
  WHERE  bukrs EQ it_zhcmt_py_0004-bukrs.

  REFRESH: tg_lotes.
  REFRESH it_estra.
  SORT: it_t001                   BY bukrs,
        it_zhcmt_py_0006          BY lote nivel aprovador.

  LOOP AT it_zhcmt_py_0004_t INTO wa_zhcmt_py_0004_t.
    xtotal = 0.
    LOOP AT it_zhcmt_py_0004 INTO wa_zhcmt_py_0004 WHERE lote = wa_zhcmt_py_0004_t-lote.
      ADD wa_zhcmt_py_0004-betrg TO xtotal.
    ENDLOOP.
    vflg_ico = 'N'.
    LOOP AT it_zhcmt_py_0005 INTO wa_zhcmt_py_0005 WHERE abkrs = wa_zhcmt_py_0004_t-abkrs.
      IF  wa_zhcmt_py_0005-bukrs_ate IS INITIAL.
        IF  wa_zhcmt_py_0005-bukrs NE wa_zhcmt_py_0004_t-bukrs.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zhcmt_py_0005-bukrs     GT wa_zhcmt_py_0004_t-bukrs OR
             wa_zhcmt_py_0005-bukrs_ate LT wa_zhcmt_py_0004_t-bukrs.
        CONTINUE.
      ENDIF.




      wa_estra-bukrs        = wa_zhcmt_py_0004_t-bukrs.
      wa_estra-lote         = wa_zhcmt_py_0004_t-lote.
      wa_estra-aprovador    = wa_zhcmt_py_0005-aprovador.
      IF wa_zhcmt_py_0005-aprovador_tmp IS NOT INITIAL.
        IF sy-datum BETWEEN wa_zhcmt_py_0005-dt_val_de_tmp AND wa_zhcmt_py_0005-dt_val_ate_tmp.
          wa_estra-aprovador = wa_zhcmt_py_0005-aprovador_tmp.
        ENDIF.
      ENDIF.
      wa_estra-nivel        = wa_zhcmt_py_0005-nivel.

      READ TABLE it_zhcmt_py_0006 INTO wa_zhcmt_py_0006 WITH KEY lote      = wa_zhcmt_py_0004_t-lote
                                                                 nivel     = wa_zhcmt_py_0005-nivel BINARY SEARCH.

      IF sy-subrc = 0.
        wa_estra-estado       = icon_checked .
        wa_estra-opcoes       = icon_system_undo .
        vflg_ico = 'N'.
        wa_estra-aprovador    = wa_zhcmt_py_0006-aprovador.
      ELSEIF vflg_ico = 'S'.
        wa_estra-estado       = icon_led_yellow .
        wa_estra-opcoes       = '' .
        wa_estra-aprovador    = wa_zhcmt_py_0005-aprovador.
        IF wa_zhcmt_py_0005-aprovador_tmp IS NOT INITIAL.
          IF sy-datum BETWEEN wa_zhcmt_py_0005-dt_val_de_tmp AND wa_zhcmt_py_0005-dt_val_ate_tmp.
            wa_estra-aprovador = wa_zhcmt_py_0005-aprovador_tmp.
          ENDIF.
        ENDIF.
      ELSE.
        IF v_usuario NE wa_estra-aprovador.
          wa_estra-estado       =  ' '.
          wa_estra-opcoes       = icon_led_yellow  .
        ELSE.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = icon_set_state  .
        ENDIF.
        vflg_ico = 'X'.
        wa_estra-aprovador    = wa_zhcmt_py_0005-aprovador.
        IF wa_zhcmt_py_0005-aprovador_tmp IS NOT INITIAL.
          IF sy-datum BETWEEN wa_zhcmt_py_0005-dt_val_de_tmp AND wa_zhcmt_py_0005-dt_val_ate_tmp.
            wa_estra-aprovador = wa_zhcmt_py_0005-aprovador_tmp.
          ENDIF.
        ENDIF.
      ENDIF.

      IF vflg_ico = 'X'.
        vflg_ico = 'S'.
      ENDIF.



      APPEND wa_estra TO it_estra.
    ENDLOOP.

    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zhcmt_py_0004_t-bukrs BINARY SEARCH.
    CONCATENATE wa_zhcmt_py_0004_t-bukrs '-' wa_t001-butxt INTO  tg_lotes-empresa.
    tg_lotes-lote       = wa_zhcmt_py_0004_t-lote.
    IF wa_zhcmt_py_0004_t-tp_pgto = 'FOLHAPGTO'.
      tg_lotes-areafpg    = '01-AMAGGI Matriz'.
    ELSE.
      tg_lotes-areafpg    = wa_zhcmt_py_0004_t-areafpg.
    ENDIF.
    tg_lotes-tp_pgto    = wa_zhcmt_py_0004_t-tp_pgto.
    tg_lotes-dt_cred    = wa_zhcmt_py_0004_t-dt_cred.
    tg_lotes-total      = xtotal.
    tg_lotes-sgtxt      = wa_zhcmt_py_0004_t-tp_pgto.
    tg_lotes-waers      = wa_t001-waers.

    APPEND tg_lotes.
    CLEAR tg_lotes.

  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM tg_lotes[] COMPARING lote.

  CLEAR:lr_lote.

  IF tg_lotes[] IS NOT INITIAL.
    SORT it_estra BY lote aprovador.
    LOOP AT tg_lotes.
      CLEAR vflag.
      LOOP AT it_estra INTO wa_estra WHERE lote      = tg_lotes-lote
                                     AND   aprovador = v_usuario.
        vflag = 'X'.
        EXIT.
      ENDLOOP.
      LOOP AT it_estra INTO wa_estra WHERE lote      = tg_lotes-lote.
        MOVE-CORRESPONDING wa_estra TO t_estra.
        APPEND t_estra.
      ENDLOOP.
      SORT  t_estra BY lote nivel.
      IF vflag = 'X'.

        APPEND VALUE #( sign = 'I'
                        option = 'EQ'
                        low = tg_lotes-lote ) TO lr_lote.

        MOVE-CORRESPONDING tg_lotes TO t_lotes.
        "CONCATENATE TG_LOTES-DT_VENC+6(2) '.' TG_LOTES-DT_VENC+4(2) '.' TG_LOTES-DT_VENC+0(4) INTO T_LOTES-DT_VENC.
        APPEND t_lotes.
      ENDIF.
    ENDLOOP.

    IF lr_lote IS NOT INITIAL.
      DELETE it_zhcmt_py_0004 WHERE lote NOT IN lr_lote.
      LOOP AT it_zhcmt_py_0004 ASSIGNING FIELD-SYMBOL(<fs_py04>).
        APPEND INITIAL LINE TO t_docs ASSIGNING FIELD-SYMBOL(<fs_lote>).
        MOVE-CORRESPONDING <fs_py04> TO <fs_lote>.
      ENDLOOP.
    ENDIF.

    IF t_lotes[] IS NOT INITIAL.
      msg = 'Sucesso'.
    ELSE.
      msg = 'Não há lotes à aprovar.'.
    ENDIF.

  ENDIF.
ENDFUNCTION.
