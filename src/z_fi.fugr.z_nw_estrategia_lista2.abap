FUNCTION z_nw_estrategia_lista2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(V_USUARIO) LIKE  SY-UNAME
*"  EXPORTING
*"     VALUE(MSG) TYPE  CHAR50
*"  TABLES
*"      T_LOTES STRUCTURE  ZFI_LOTES_IMP
*"      T_ESTRA STRUCTURE  ZFI_ESTRATEGIA_IMP
*"      T_DOCS STRUCTURE  ZGL_DOCS_IMP
*"      T_DOCS_ENERGIA STRUCTURE  ZGL_DOCS_ENERGIA OPTIONAL
*"      T_ZFIWRT0022 STRUCTURE  ZFIWRT0022 OPTIONAL
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
  TYPE-POOLS: icon.

  TYPES:

    BEGIN OF ty_t001,
      bukrs TYPE t001-bukrs,
      butxt TYPE t001-butxt,
      land1 TYPE t001-land1,
    END OF ty_t001,

    BEGIN OF ty_kna1,
      kunnr TYPE kna1-kunnr,
      name1 TYPE kna1-name1,
    END OF ty_kna1,

    BEGIN OF ty_lfa1,
      lifnr TYPE lfa1-kunnr,
      name1 TYPE lfa1-name1,
    END OF ty_lfa1,

    BEGIN OF ty_estra ,
      bukrs     TYPE zglt038-bukrs,
      lote      TYPE zglt038-lote,
      valor_de  TYPE zglt037-valor_de,
      valor_ate TYPE zglt037-valor_ate,
      aprovador TYPE zglt037-aprovador,
      nivel     TYPE zglt037-nivel,
      estado(4),
      opcoes(4),
    END OF ty_estra,

*    BEGIN OF ty_docs ,
*      doc_lcto        TYPE zglt035-doc_lcto,
*      bukrs           TYPE zglt035-bukrs,
*      tp_lcto         TYPE zglt035-tp_lcto,
*      descricao       TYPE zglt031-descricao,
*      lote            TYPE zglt035-lote,
*      moeda_doc       TYPE zglt035-moeda_doc,
*      vlr_moeda_int   TYPE zglt036-vlr_moeda_int,
*      vlr_moeda_forte TYPE zglt036-vlr_moeda_forte,
*    END OF ty_docs,

    BEGIN OF ty_libe,
      nivel_aprov TYPE zfiwrt0007-nivel_aprov,
    END   OF ty_libe.

  TYPES: BEGIN OF ty_docs_aux,
           doc_lcto TYPE zfiwrt0022-seq_lcto.
  TYPES: END OF ty_docs_aux.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
  DATA: xtotal   TYPE zglt036-vlr_moeda_int,
        xtotalf  TYPE zglt036-vlr_moeda_forte,
        vbranch  TYPE zfiwrt0008-branch,
        vflag(1),

        BEGIN OF tg_lotes OCCURS 0,
          status(4),
          empresa(30)  TYPE c,
          lote         TYPE zfiwrt0008-seq_lcto,
          dep_resp(25) TYPE c,
          total        TYPE zglt036-vlr_moeda_int,
          dt_venc      TYPE zfiwrt0008-budat,
          color(4),
        END OF tg_lotes.

** Criação de tabela dinamica
  DATA:

    wa_zfiwrt0008   TYPE zfiwrt0008,
    wa_zfiwrt0009   TYPE zfiwrt0009,
    wa_zfiwrt0007   TYPE zfiwrt0007,
    wa_zfiwrt0018   TYPE zfiwrt0018,

    wa_lfa1         TYPE ty_lfa1,
    wa_kna1         TYPE ty_kna1,
    wa_t001         TYPE ty_t001,
    wa_estra        TYPE ty_estra,
    wa_docs         TYPE ZGL_DOCS_IMP,
    wa_docs_aux     TYPE ty_docs_aux,
    wa_docs_energia TYPE zgl_docs_energia,
    wa_libe         TYPE ty_libe,
    it_libe         TYPE TABLE OF ty_libe,

    tg_docs         TYPE TABLE OF ZGL_DOCS_IMP,
    tg_docs_aux     TYPE TABLE OF ty_docs_aux,

    it_lfa1         TYPE TABLE OF ty_lfa1,
    it_kna1         TYPE TABLE OF ty_kna1,
    it_t001         TYPE TABLE OF ty_t001,
    it_zfiwrt0008   TYPE TABLE OF zfiwrt0008,
    it_zfiwrt0009   TYPE TABLE OF zfiwrt0009,
    it_zfiwrt0007   TYPE TABLE OF zfiwrt0007,
    it_zfiwrt0018   TYPE TABLE OF zfiwrt0018,

    it_estra        TYPE TABLE OF ty_estra.


  DATA vflg_ico(1).

  FREE: t_docs_energia.

  CHECK t_zfiwrt0022[] IS NOT INITIAL.

  SELECT *
    FROM zfiwrt0008
    INTO TABLE  it_zfiwrt0008
     FOR ALL ENTRIES IN t_zfiwrt0022
   WHERE seq_lcto    EQ t_zfiwrt0022-seq_lcto
     AND status      EQ space
     AND loekz       EQ space
     AND EXISTS ( SELECT * FROM zfiwrt0001 WHERE zfiwrt0001~operacao = zfiwrt0008~operacao AND zfiwrt0001~lm_aprova NE 'N' )
     AND EXISTS ( SELECT operacao FROM zfiwrt0007 WHERE zfiwrt0007~operacao = zfiwrt0008~operacao ).

  SELECT *
    FROM zfiwrt0008
    APPENDING TABLE  it_zfiwrt0008
     FOR ALL ENTRIES IN t_zfiwrt0022
   WHERE   seq_lcto  EQ t_zfiwrt0022-seq_lcto
     AND ( status    EQ 'P'
      OR   status    EQ 'R'
      OR   status    EQ 'A' )
     AND   loekz     EQ abap_false.
*    AND EXISTS ( SELECT * FROM zfiwrt0001 WHERE zfiwrt0001~operacao = zfiwrt0008~operacao AND zfiwrt0001~lm_aprova EQ 'N' )
*    AND EXISTS ( SELECT operacao FROM zfiwrt0007 WHERE zfiwrt0007~operacao = zfiwrt0008~operacao ).

  CHECK it_zfiwrt0008[] IS NOT INITIAL.

  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
     FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE kunnr EQ it_zfiwrt0008-parid.

  SELECT lifnr name1
    FROM lfa1
    INTO TABLE it_lfa1
     FOR ALL ENTRIES IN it_zfiwrt0008
     WHERE lifnr EQ it_zfiwrt0008-parid.

  SELECT bukrs butxt land1
    FROM t001
    INTO TABLE it_t001
    FOR ALL ENTRIES IN it_zfiwrt0008
    WHERE  bukrs EQ it_zfiwrt0008-bukrs.

  SELECT  *
    FROM zfiwrt0009
    INTO TABLE it_zfiwrt0009
    FOR ALL ENTRIES IN it_zfiwrt0008
    WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto.

  SELECT  *
    FROM zfiwrt0018
    INTO TABLE it_zfiwrt0018
    FOR ALL ENTRIES IN it_zfiwrt0008
    WHERE seq_lcto EQ it_zfiwrt0008-seq_lcto.

  SELECT  *
    FROM zfiwrt0007
    INTO TABLE it_zfiwrt0007
    WHERE tipo = 'W'.

  SORT it_zfiwrt0007 BY nivel_aprov.

  SORT: it_t001             BY bukrs,
        it_zfiwrt0007       BY operacao branch nivel_aprov usnam,
        it_zfiwrt0008       BY seq_lcto ,
        it_zfiwrt0009       BY seq_lcto,
        it_zfiwrt0018       BY seq_lcto operacao branch nivel_aprov usnam.


  REFRESH: tg_lotes, tg_docs.
  REFRESH it_estra.
  LOOP AT it_zfiwrt0008 INTO wa_zfiwrt0008.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfiwrt0008-bukrs BINARY SEARCH.
    CONCATENATE wa_zfiwrt0008-bukrs '-' wa_t001-butxt INTO  tg_lotes-empresa.
    tg_lotes-lote       = wa_zfiwrt0008-seq_lcto.

    IF wa_zfiwrt0008-parvw EQ 'AG'.
      READ TABLE it_kna1 INTO wa_kna1
        WITH KEY kunnr = wa_zfiwrt0008-parid.

      MOVE: wa_kna1-name1 TO tg_lotes-dep_resp.

    ELSEIF wa_zfiwrt0008-parvw EQ 'LF'
        OR wa_zfiwrt0008-parvw EQ 'BR'.
      READ TABLE it_lfa1 INTO wa_lfa1
        WITH KEY lifnr = wa_zfiwrt0008-parid.

      MOVE: wa_lfa1-name1 TO tg_lotes-dep_resp.

    ENDIF.

    xtotal = 0.
    LOOP AT it_zfiwrt0009 INTO wa_zfiwrt0009 WHERE seq_lcto = wa_zfiwrt0008-seq_lcto.
      xtotal = xtotal + wa_zfiwrt0009-netwr.
    ENDLOOP.

    tg_lotes-total   = xtotal.
    tg_lotes-dt_venc = wa_zfiwrt0008-budat.

    READ TABLE it_zfiwrt0007 INTO wa_zfiwrt0007 WITH KEY  operacao = wa_zfiwrt0008-operacao
                                                          branch   = wa_zfiwrt0008-branch.
    IF sy-subrc NE 0. "não tem estrategia por divisao
      CLEAR vbranch.
    ELSE.
      vbranch = wa_zfiwrt0008-branch.
    ENDIF.

*---CS2020000744 - 28.12.2020 - inicio
    FREE: it_libe.
    LOOP AT it_zfiwrt0007 INTO wa_zfiwrt0007 WHERE operacao = wa_zfiwrt0008-operacao
                                               AND branch   = vbranch
                                               AND usnam    = v_usuario.
      CLEAR wa_libe.
      wa_libe-nivel_aprov = wa_zfiwrt0007-nivel_aprov.
      APPEND wa_libe     TO it_libe.
    ENDLOOP.

    SORT it_libe BY nivel_aprov.
    DELETE ADJACENT DUPLICATES FROM it_libe
                          COMPARING nivel_aprov.
*---CS2020000744 - 28.12.2020 - fim

    CLEAR vflg_ico.
    LOOP AT it_zfiwrt0007 INTO wa_zfiwrt0007 WHERE operacao = wa_zfiwrt0008-operacao
                                             AND   branch   = vbranch.

*---CS2020000744 - 28.12.2020 - inicio
      CLEAR wa_libe.
      READ TABLE it_libe INTO wa_libe WITH KEY nivel_aprov = wa_zfiwrt0007-nivel_aprov
                                      BINARY SEARCH.
      IF sy-subrc = 0 AND wa_zfiwrt0007-usnam <> v_usuario.
        CONTINUE.
      ENDIF.
*---CS2020000744 - 28.12.2020 - fim

      READ TABLE it_zfiwrt0018   INTO wa_zfiwrt0018 WITH KEY seq_lcto      = wa_zfiwrt0008-seq_lcto
                                                             operacao      = wa_zfiwrt0008-operacao
                                                             branch        = wa_zfiwrt0008-branch
                                                             nivel_aprov   = wa_zfiwrt0007-nivel_aprov BINARY SEARCH. " BUG 50742
*                                                             usnam         = wa_zfiwrt0007-usnam BINARY SEARCH.

      IF sy-subrc = 0.
        IF ( wa_zfiwrt0018-usnam = wa_zfiwrt0007-usnam ).
          wa_estra-estado       = icon_checked .
          wa_estra-opcoes       = icon_system_undo .
          vflg_ico = 'N'.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSEIF vflg_ico = 'S'.
        wa_estra-estado       = icon_led_yellow .
        wa_estra-opcoes       = '' .
      ELSE.
        IF v_usuario NE wa_zfiwrt0007-usnam.
          wa_estra-estado       =  ' '.
          wa_estra-opcoes       = icon_led_yellow  .
        ELSE.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = icon_set_state  .
        ENDIF.
        vflg_ico = 'X'.
      ENDIF.

      IF wa_zfiwrt0008-status = 'R'.
        wa_estra-estado       = icon_reject.
        wa_estra-opcoes       = icon_set_state  .
      ENDIF.

      IF vflg_ico = 'X'.
        vflg_ico = 'S'.
      ENDIF.

      wa_estra-bukrs        = wa_zfiwrt0008-bukrs.
      wa_estra-lote         = wa_zfiwrt0008-seq_lcto.
      wa_estra-aprovador    = wa_zfiwrt0007-usnam.
      wa_estra-nivel        = wa_zfiwrt0007-nivel_aprov.

      APPEND wa_estra TO it_estra.
    ENDLOOP.


    APPEND tg_lotes.
    CLEAR tg_lotes.
  ENDLOOP.

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
      IF vflag = 'X'.
        LOOP AT it_zfiwrt0009 INTO wa_zfiwrt0009 WHERE seq_lcto = tg_lotes-lote.
          wa_docs-doc_lcto          = wa_zfiwrt0009-seq_lcto.
          wa_docs-vlr_moeda_int     = wa_zfiwrt0009-netwr.
          wa_docs-descricao         = wa_zfiwrt0009-matnr.
          wa_docs-lote              = wa_zfiwrt0009-seq_lcto.
          APPEND wa_docs TO t_docs.
        ENDLOOP.
        MOVE-CORRESPONDING tg_lotes TO t_lotes.
        CONCATENATE tg_lotes-dt_venc+6(2) '.' tg_lotes-dt_venc+4(2) '.' tg_lotes-dt_venc+0(4) INTO t_lotes-dt_venc.

        APPEND t_lotes.
      ENDIF.
    ENDLOOP.


    IF t_lotes[] IS NOT INITIAL.
      msg = 'Sucesso'.
    ELSE.
      msg = 'Não há lotes à aprovar.'.
    ENDIF.

  ENDIF.

*-CS2021000723 - 18.10.2021 - JT - inicio
  PERFORM f_doumentos_energia TABLES t_docs
                                     t_docs_energia.
*-CS2021000723 - 18.10.2021 - JT - fim

ENDFUNCTION.
