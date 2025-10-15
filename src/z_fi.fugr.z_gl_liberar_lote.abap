FUNCTION z_gl_liberar_lote.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_NUM_LOTE) TYPE  ZLOTE_NUM
*"  EXPORTING
*"     REFERENCE(P_LIBERADO) TYPE  CHAR01
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_estra ,
           bukrs     TYPE zglt038-bukrs,
           lote      TYPE zglt038-lote,
           valor_de  TYPE zglt037-valor_de,
           valor_ate TYPE zglt037-valor_ate,
           aprovador TYPE zglt037-aprovador,
           nivel     TYPE zglt037-nivel,
           estado(4),
           opcoes(4),
         END OF ty_estra,

         BEGIN OF ty_cadlote,
           empresa(30)        TYPE c,
           lote(50)           TYPE c,
           usuario(20)        TYPE c,
           total              TYPE zglt036-vlr_moeda_int,
           dep_resp(2),
           data(10),
           " Seguro
           tp_opr(30),
           vig_de             TYPE zglt050-vig_de,
           vig_ate            TYPE zglt050-vig_ate,
           cod_seguradora(50),
           seq_parc           TYPE zglt050-seq_parc,
           seq_tipo(45),
           observacao         TYPE zglt050-observacao,
         END OF ty_cadlote.

  DATA: vl_lote    TYPE zglt034,
        wl_zglt035 TYPE zglt035,
        tl_zglt036 TYPE TABLE OF zglt036,
        tl_tbsl    TYPE TABLE OF tbsl.

  DATA: v_msg      TYPE char50,
        t_lotes    TYPE TABLE OF zfi_lotes_imp,
        w_lotes    TYPE          zfi_lotes_imp,
        vg_lote    TYPE          zglt034-lote,

        t_estra    TYPE TABLE OF zfi_estrategia_zgl,
        w_estra    TYPE          zfi_estrategia_zgl,

        t_docs     TYPE TABLE OF zgl_docs_imp,
        w_docs     TYPE          zgl_docs_imp,

        tg_estra   TYPE TABLE OF zfi_estrategia_imp,
        wg_estra   TYPE zfi_estrategia_imp,
        wg_cadlote TYPE ty_cadlote,
        wl_t001    TYPE t001.

  DATA: wa_zglt090 TYPE zglt090,
        it_zglt090 TYPE TABLE OF zglt090.

  CLEAR: vl_lote.

  SELECT SINGLE * FROM zglt034 INTO vl_lote WHERE lote = p_num_lote.

  CHECK sy-subrc = 0.

  IF vl_lote-status_lote = ' ' OR ( vl_lote-status_lote = 'L' AND (  sy-tcode EQ 'ZGL076' OR sy-tcode EQ 'ZGL081' OR sy-tcode EQ 'ZGL082'  "bug #111987 - Transaçãoes ja vão criar o lote como liberado mas precisa gravar na zib_contabil
                                                                    OR sy-tcode = 'ZGL083' ) ). " Equivalenciam Patrimonial -
    IF vl_lote-status_lote = ' '.
      UPDATE zglt034
         SET status_lote = 'L' "Modificado de L para A //==BUG SOLTO 103403 / AOENNING / 23-02/2023 ------ Dia 10-05-2023 voltamos para alterar Status para L
       WHERE lote EQ p_num_lote.
      COMMIT WORK.
    ELSE.
      UPDATE zglt034
       SET status_lote = 'A'
     WHERE lote EQ p_num_lote.
      COMMIT WORK.
    ENDIF.



    SELECT SINGLE *
      FROM t001
      INTO wl_t001
     WHERE bukrs EQ vl_lote-bukrs.

    "Verificar se é de fornecedor
    REFRESH: tl_tbsl, tl_zglt036.
    SELECT *
      FROM zglt035
      INNER JOIN zglt036
              ON zglt036~doc_lcto EQ zglt035~doc_lcto
       INTO CORRESPONDING FIELDS OF TABLE tl_zglt036
    WHERE zglt035~lote  EQ  p_num_lote.

    IF tl_zglt036[] IS NOT INITIAL.
      SELECT *
        FROM tbsl
        INTO TABLE tl_tbsl
        FOR ALL ENTRIES IN  tl_zglt036
        WHERE bschl EQ  tl_zglt036-bschl
          AND koart IN ('K', 'D').
    ENDIF.

    READ TABLE tl_zglt036 INTO DATA(wl_036) WITH KEY estrategia_forn = 'X'.
    IF sy-subrc = 0.
      DATA(_estrategia_forn) = 'X'.
    ENDIF.

    "Email
    CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
      EXPORTING
        v_usuario = sy-uname
        v_lote    = p_num_lote
      IMPORTING
        msg       = v_msg
      TABLES
        t_lotes   = t_lotes
        t_estra   = t_estra
        t_docs    = t_docs.

    REFRESH tg_estra .
    SORT t_estra BY nivel.
    LOOP AT t_estra INTO w_estra.
      MOVE-CORRESPONDING w_estra TO wg_estra.
      APPEND wg_estra TO tg_estra.
      MOVE-CORRESPONDING wg_estra TO wa_zglt090.
      wa_zglt090-data_atual = sy-datum.
      wa_zglt090-hora_atual = sy-uzeit.
      wa_zglt090-usuario    = sy-uname.
      APPEND wa_zglt090 TO it_zglt090.
    ENDLOOP.
    "
    " grava estratégia
    DELETE FROM zglt090 WHERE lote = p_num_lote.
    MODIFY zglt090 FROM TABLE it_zglt090.
    COMMIT WORK."

    IF ( tl_tbsl[] IS INITIAL ) AND ( _estrategia_forn IS INITIAL ). "Se nenhum lançamento for fornecedor/cliente

      SELECT SINGLE *
        FROM zglt035 INTO wl_zglt035
       WHERE lote     EQ p_num_lote
         AND prov_est EQ 'X'.

      IF sy-subrc = 0.
        PERFORM grava_zib(zgl017) USING p_num_lote.
      ELSE.
        SUBMIT z_grava_zib_zgl WITH p_lote = p_num_lote AND RETURN.
      ENDIF.

      p_liberado = 'X'.

    ELSE.

      CONCATENATE wl_t001-bukrs '-' wl_t001-butxt      INTO wg_cadlote-empresa.
      CONCATENATE p_num_lote    '-' vl_lote-descr_lote INTO wg_cadlote-lote.

      wg_cadlote-total    = 0.
      wg_cadlote-dep_resp = vl_lote-dep_resp.

      PERFORM envia_email(zgl017) TABLES tg_estra USING wg_cadlote 0.

      p_liberado = 'X'.

    ENDIF.

  ELSE.
    p_liberado = 'X'.
  ENDIF.


ENDFUNCTION.
