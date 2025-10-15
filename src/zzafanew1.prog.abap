*&---------------------------------------------------------------------*
*& Report  ZZAFANEW1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zzafanew1.

TYPE-POOLS: abap.

TABLES: anla,
        anlb,
        anlbza,
        anlc,
        anep,
        anea,
        anli,
        auai,
        auak,
        coepbr.

TYPES: BEGIN OF y_upload,
         anln1 TYPE anla-anln1,
         anln2 TYPE anla-anln2,
         xafabe TYPE anlb-afabe,
         yafabe TYPE anlb-afabe,
       END OF y_upload.

DATA: gt_anla LIKE anla OCCURS 0 WITH HEADER LINE.
DATA: gt_anlb TYPE anlb OCCURS 0 WITH HEADER LINE,
      gt_anlbza TYPE anlbza OCCURS 0 WITH HEADER LINE,
      gt_anlc TYPE anlc OCCURS 0 WITH HEADER LINE,
      gt_anep TYPE anep OCCURS 0 WITH HEADER LINE,
      gt_anea TYPE anea OCCURS 0 WITH HEADER LINE,
      gt_auak TYPE auak OCCURS 0 WITH HEADER LINE,
      gt_auai TYPE auai OCCURS 0 WITH HEADER LINE,
      t_upload TYPE y_upload OCCURS 0 WITH HEADER LINE.

DATA: v_erro1.
CONSTANTS: c_x TYPE c VALUE 'X',
           c_0000(4) TYPE c VALUE '0000'.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE scrtit.
*SELECTION-SCREEN COMMENT /1(60) bukrs.
SELECT-OPTIONS: xbukrs FOR anlb-bukrs.
*SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN COMMENT /1(60) anln1.
SELECT-OPTIONS: xanln1 FOR anlb-anln1.
*SELECTION-SCREEN COMMENT /1(60) anln2.
SELECT-OPTIONS: xanln2 FOR anlb-anln2.
*SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN COMMENT /1(60) safabe.
PARAMETERS: xafabe LIKE anlb-afabe. "OBLIGATORY.    "Source
*SELECTION-SCREEN COMMENT /1(60) tafabe.
PARAMETERS: yafabe LIKE anlb-afabe. "OBLIGATORY.    "Target
*SELECTION-SCREEN SKIP 1.
*SELECTION-SCREEN COMMENT /1(60) test.
PARAMETERS: xflag AS CHECKBOX DEFAULT abap_true,
            p_mov AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK 1.
*Inicio da alteração - TRRE
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE arq.
PARAMETERS: p_arq_ch AS CHECKBOX USER-COMMAND scr,
            p_arq LIKE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK 2.


*----------------------------------------------------------------------*
* At Selection-Screen -------------------------------------------------*
*----------------------------------------------------------------------*
* Concistência tela de seleção
AT SELECTION-SCREEN OUTPUT.
  PERFORM valida_tela_selecao.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = ',*.*,*.*.'
      mode             = 's'
      title            = 'Arquivo de Saída'
    IMPORTING
      filename         = p_arq
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

*Fim da Alteração - TRRE
********************** initialization **********************************
INITIALIZATION.
  MOVE 'Abertura automática de uma nova área de avaliação' TO scrtit.
  "MOVE 'Empresa'                         TO xbukrs.
  "MOVE 'Numero do ativo'                 TO xanln1.
  "MOVE 'Subnumero'                       TO xanln2.
  "MOVE 'Área de Avaliação - ORGIGEM'     TO xafabe.
  "MOVE 'Área de Avaliação - DESTINO'     TO yafabe.
*  MOVE 'Execução TESTE'                  TO test.


********************** start selecting data ****************************
START-OF-SELECTION.

*Inicio da Alteração - TRRE
  IF p_arq_ch IS NOT INITIAL.
    PERFORM carrega_arq.

    SELECT * FROM anla INTO TABLE gt_anla
      FOR ALL ENTRIES IN t_upload
                       WHERE bukrs IN xbukrs
                         AND anln1 = t_upload-anln1
                         AND anln2 = t_upload-anln2.

  ELSE.

    IF xafabe IS INITIAL OR
       yafabe IS INITIAL.
      MESSAGE i000(z01) WITH 'Informar áreas de origem/destino.'.
      v_erro1 = c_x.
    ENDIF.

    SELECT * FROM anla INTO TABLE gt_anla
                       WHERE bukrs IN xbukrs
                         AND anln1 IN xanln1
                         AND anln2 IN xanln2.
  ENDIF.
  IF v_erro1 IS INITIAL.
    SELECT * FROM anlb INTO TABLE gt_anlb
      FOR ALL ENTRIES IN gt_anla
        WHERE bukrs IN xbukrs
          AND anln1 = gt_anla-anln1
          AND anln2 = gt_anla-anln2.

    SELECT * FROM anlbza INTO TABLE gt_anlbza
      FOR ALL ENTRIES IN gt_anla
        WHERE bukrs = gt_anla-bukrs
          AND anln1 = gt_anla-anln1
          AND anln2 = gt_anla-anln2.

    SELECT * FROM anlc INTO TABLE gt_anlc
      FOR ALL ENTRIES IN gt_anla
        WHERE bukrs = gt_anla-bukrs
*                     AND afabe = xafabe
          AND anln1 = gt_anla-anln1
          AND anln2 = gt_anla-anln2.

    IF p_mov IS INITIAL.
      SELECT * FROM anep INTO TABLE gt_anep
        FOR ALL ENTRIES IN gt_anla
          WHERE bukrs = gt_anla-bukrs
*                  AND afabe = xafabe
            AND anln1 = gt_anla-anln1
            AND anln2 = gt_anla-anln2.
    ENDIF.
    SELECT * FROM anea INTO TABLE gt_anea
      FOR ALL ENTRIES IN gt_anla
        WHERE bukrs = gt_anla-bukrs
*                  AND afabe = xafabe
          AND anln1 = gt_anla-anln1
          AND anln2 = gt_anla-anln2.

*                       AND afabe = xafabe.

    PERFORM trata_dados.
    IF NOT xflag = abap_true.
      PERFORM atualiza_dados.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_tela_selecao .
  IF p_arq_ch = c_x.
    LOOP AT SCREEN.

      IF screen-name = 'P_ARQ'.

        screen-input = '1'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'XAFABE' OR
             screen-name = 'YAFABE' OR
             screen-name = 'XANLN1-LOW' OR
             screen-name = 'XANLN1-HIGH' OR
             screen-name = 'XANLN2-LOW' OR
             screen-name = 'XANLN2-HIGH'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.

      IF screen-name = 'P_ARQ'.

        screen-input = '0'.
        MODIFY SCREEN.
      ELSEIF screen-name = 'XAFABE' OR
             screen-name = 'YAFABE' OR
             screen-name = 'XANLN1-LOW' OR
             screen-name = 'XANLN1-HIGH' OR
             screen-name = 'XANLN2-LOW' OR
             screen-name = 'XANLN2-HIGH'.
        screen-input = '1'.
        MODIFY SCREEN.
      ENDIF.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM carrega_arq .
  DATA: t_aux TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_arq
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 4
      i_end_row               = 65536
    TABLES
      intern                  = t_aux
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.

*    MESSAGE i000 WITH text-e11 p_arq.
*    STOP.

  ELSE.

    LOOP AT t_aux.

      CASE t_aux-col.

        WHEN 1.
          t_upload-anln1 = t_aux-value.
          SHIFT t_upload-anln1 RIGHT DELETING TRAILING space.
          TRANSLATE t_upload-anln1 USING ' 0' .

        WHEN 2.
          t_upload-anln2 = t_aux-value.
          SHIFT t_upload-anln2 RIGHT DELETING TRAILING space.
          TRANSLATE t_upload-anln2 USING ' 0' .

        WHEN 3.
          t_upload-xafabe = t_aux-value.
          SHIFT t_upload-xafabe RIGHT DELETING TRAILING space.
          TRANSLATE t_upload-xafabe USING ' 0' .

        WHEN 4.
          t_upload-yafabe = t_aux-value.
          SHIFT t_upload-yafabe RIGHT DELETING TRAILING space.
          TRANSLATE t_upload-yafabe USING ' 0' .

          APPEND t_upload.
          CLEAR t_upload.

      ENDCASE.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " CARREGA_ARQ
*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trata_dados .

  DATA: v_erro.
  IF xflag = abap_true.
    WRITE: / '*** Teste! ***'.
  ELSE.
    WRITE: / '*** ATUALIZAÇÃO EFETIVA! ***'.
  ENDIF.
  IF p_arq_ch IS NOT INITIAL.
    LOOP AT t_upload.
      READ TABLE gt_anla WITH KEY anln1 = t_upload-anln1
                                  anln2 = t_upload-anln2.
      IF sy-subrc IS INITIAL.
        PERFORM altera_anlb USING t_upload-anln1 t_upload-anln2
                                  t_upload-xafabe t_upload-yafabe
                            CHANGING v_erro.
        IF v_erro IS INITIAL.
          PERFORM altera_anlbza USING t_upload-anln1 t_upload-anln2
                                      t_upload-xafabe t_upload-yafabe
                                CHANGING v_erro.
          IF v_erro IS INITIAL.
            PERFORM altera_anlc USING  t_upload-anln1 t_upload-anln2
                                       t_upload-xafabe t_upload-yafabe.
            IF NOT p_mov IS NOT INITIAL.
              PERFORM altera_anep USING t_upload-anln1 t_upload-anln2
                                        t_upload-xafabe t_upload-yafabe.
            ENDIF.

            IF NOT p_mov IS NOT INITIAL.
              PERFORM altera_anea USING t_upload-anln1 t_upload-anln2
                                      t_upload-xafabe t_upload-yafabe.
            ENDIF.

          ELSE.
            CLEAR v_erro.
            CONTINUE.
          ENDIF.
        ELSE.
          CLEAR v_erro.
          CONTINUE.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ELSE.

    LOOP AT gt_anla.
      PERFORM altera_anlb USING gt_anla-anln1 gt_anla-anln2
                              xafabe yafabe
                        CHANGING v_erro.
      IF v_erro IS INITIAL.
        PERFORM altera_anlbza USING gt_anla-anln1 gt_anla-anln2
                                    xafabe yafabe
                              CHANGING v_erro.
        IF v_erro IS INITIAL.
          PERFORM altera_anlc USING  gt_anla-anln1 gt_anla-anln2
                                     xafabe yafabe.
          IF p_mov IS INITIAL.
*        if not p_mov is initial.
            PERFORM altera_anep USING gt_anla-anln1 gt_anla-anln2
                                      xafabe yafabe.
          ENDIF.

          IF p_mov IS INITIAL.
            PERFORM altera_anea USING gt_anla-anln1 gt_anla-anln2
                                    xafabe yafabe.
          ENDIF.
        ELSE.
          CLEAR v_erro.
          CONTINUE.
        ENDIF.
      ELSE.
        CLEAR v_erro.
        CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " TRATA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ANLB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_UPLOAD_ANLN1  text
*      -->P_T_UPLOAD_ANLN2  text
*      -->P_T_UPLOAD_XAFABE  text
*      -->P_T_UPLOAD_YAFABE  text
*      <--P_V_ERRO  text
*----------------------------------------------------------------------*
FORM altera_anlb  USING    p_anln1
                           p_anln2
                           p_xafabe
                           p_yafabe
                  CHANGING p_erro.
  READ TABLE gt_anlb WITH KEY anln1 = p_anln1
                              anln2 = p_anln2
                              afabe = p_yafabe.
  IF sy-subrc IS INITIAL.
    WRITE: / gt_anla-bukrs, gt_anla-anln1, gt_anla-anln2,
             'Area já existe na tabela ANLB (não atualizar):', t_upload-yafabe.
    p_erro = c_x.
  ELSE.
    READ TABLE gt_anlb WITH KEY anln1 = p_anln1
                                anln2 = p_anln2
                                afabe = p_xafabe.

    IF sy-subrc IS INITIAL.
      gt_anlb-afabe = p_yafabe.
      IF NOT p_mov IS INITIAL.
        gt_anlb-afasl = c_0000.
      ENDIF.
      APPEND gt_anlb.
      WRITE: / 'ANLB-registro criado:',
               gt_anlb-bukrs,
               gt_anlb-anln1,
               gt_anlb-anln2,
               gt_anlb-afabe,
               sy-uname.
      CLEAR gt_anlb.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALTERA_ANLB
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ANLBZA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_UPLOAD_ANLN1  text
*      -->P_T_UPLOAD_ANLN2  text
*      -->P_T_UPLOAD_XAFABE  text
*      -->P_T_UPLOAD_YAFABE  text
*      <--P_V_ERRO  text
*----------------------------------------------------------------------*
FORM altera_anlbza  USING    p_anln1
                             p_anln2
                             p_xafabe
                             p_yafabe
                    CHANGING p_erro.
  READ TABLE gt_anlbza WITH KEY anln1 = p_anln1
                                anln2 = p_anln2
                                afabe = p_yafabe.
  IF sy-subrc IS INITIAL.
    WRITE: / gt_anla-bukrs, p_anln1, p_anln2,
             'Area já existe na tabela ANLB (não atualizar):', p_yafabe.
    p_erro = c_x.
  ELSE.
    READ TABLE gt_anlbza WITH KEY anln1 = p_anln1
                                  anln2 = p_anln2
                                  afabe = p_xafabe.
    IF sy-subrc IS INITIAL.
      gt_anlbza-afabe = p_yafabe.
      APPEND gt_anlbza.
      WRITE: / 'ANLBZA-registro criado:',
               gt_anlbza-bukrs,
               gt_anlbza-anln1,
               gt_anlbza-anln2,
               gt_anlbza-afabe,
               sy-uname,
               sy-datum.
      CLEAR gt_anlbza.
    ENDIF.
  ENDIF.
ENDFORM.                    " ALTERA_ANLBZA
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ANLC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_UPLOAD_ANLN1  text
*      -->P_T_UPLOAD_ANLN2  text
*      -->P_T_UPLOAD_XAFABE  text
*      -->P_T_UPLOAD_YAFABE  text
*----------------------------------------------------------------------*
FORM altera_anlc  USING    p_anln1
                           p_anln2
                           p_xafabe
                           p_yafabe.
*  read table gt_anlc with key anln1 = p_anln1
*                              anln2 = p_anln2
*                              afabe = p_xafabe.
  LOOP AT gt_anlc WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.
* if sy-subrc is initial.
    gt_anlc-afabe = p_yafabe.
    IF p_mov IS NOT INITIAL.
      CLEAR: gt_anlc-kansw,
             gt_anlc-kaufw,
             gt_anlc-kinvz,
             gt_anlc-knafa,
             gt_anlc-ksafa,
             gt_anlc-kaafa,
             gt_anlc-kmafa,
             gt_anlc-kzinw,
             gt_anlc-kaufn,
             gt_anlc-kanza,
             gt_anlc-kvost,
             gt_anlc-aufwp,
             gt_anlc-nafap,
             gt_anlc-safap,
             gt_anlc-aafap,
             gt_anlc-mafap,
             gt_anlc-zinsp,
             gt_anlc-aufnp,
             gt_anlc-aufwb,
             gt_anlc-nafag,
             gt_anlc-safag,
             gt_anlc-aafag,
             gt_anlc-mafag,
             gt_anlc-zinsg,
             gt_anlc-aufng,
             gt_anlc-answl,
             gt_anlc-abgan,
             gt_anlc-ansaz,
             gt_anlc-aufwm,
             gt_anlc-invzm,
             gt_anlc-nafam,
             gt_anlc-safam,
             gt_anlc-aafam,
             gt_anlc-mafam,
             gt_anlc-zinsm,
             gt_anlc-aufnm,
             gt_anlc-zusna,
             gt_anlc-zussa,
             gt_anlc-zusaa,
             gt_anlc-zusma,
             gt_anlc-aufwv,
             gt_anlc-invzv,
             gt_anlc-nafav,
             gt_anlc-safav,
             gt_anlc-aafav,
             gt_anlc-mafav,
             gt_anlc-aufnv,
             gt_anlc-aufwl,
             gt_anlc-invzl,
             gt_anlc-nafal,
             gt_anlc-safal,
             gt_anlc-aafal,
             gt_anlc-mafal,
             gt_anlc-aufnl.
    ENDIF.
    APPEND gt_anlc.
    WRITE: / 'ANLC-registro criado:',
           gt_anlc-bukrs,
           gt_anlc-anln1,
           gt_anlc-anln2,
           gt_anlc-afabe,
           gt_anlc-gjahr.
*           sy-datum(4).
    CLEAR gt_anlc.

*  endif.
  ENDLOOP.
ENDFORM.                    " ALTERA_ANLC
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ANEP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_UPLOAD_ANLN1  text
*      -->P_T_UPLOAD_ANLN2  text
*      -->P_T_UPLOAD_XAFABE  text
*      -->P_T_UPLOAD_YAFABE  text
*----------------------------------------------------------------------*
FORM altera_anep  USING    p_anln1
                           p_anln2
                           p_xafabe
                           p_yafabe.

*  read table gt_anep with key anln1 = p_anln1
*                              anln2 = p_anln2
*                              afabe = p_xafabe.
  LOOP AT gt_anep WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.
* if sy-subrc is initial.
    gt_anep-afabe = p_yafabe.
    APPEND gt_anep.
    WRITE: / 'ANEP-registro criado:',
           gt_anep-bukrs,
           gt_anep-anln1,
           gt_anep-anln2,
           gt_anep-afabe,
           gt_anep-gjahr,
           gt_anep-belnr.
    CLEAR gt_anep.
* endif.
  ENDLOOP.
ENDFORM.                    " ALTERA_ANEP
*&---------------------------------------------------------------------*
*&      Form  ALTERA_ANEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_UPLOAD_ANLN1  text
*      -->P_T_UPLOAD_ANLN2  text
*      -->P_T_UPLOAD_XAFABE  text
*      -->P_T_UPLOAD_YAFABE  text
*----------------------------------------------------------------------*
FORM altera_anea  USING    p_anln1
                           p_anln2
                           p_xafabe
                           p_yafabe.

*  read table gt_anea with key anln1 = p_anln1
*                              anln2 = p_anln2
*                              afabe = p_xafabe.
  LOOP AT gt_anea WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.
    IF sy-subrc IS INITIAL.
      gt_anea-afabe = p_yafabe.
      APPEND gt_anea.
      WRITE: / 'ANEA-registro criado:',
             gt_anea-bukrs,
             gt_anea-anln1,
             gt_anea-anln2,
             gt_anea-afabe,
             gt_anea-gjahr,
             gt_anea-lnran.
      CLEAR gt_anea.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ALTERA_ANEA
*&---------------------------------------------------------------------*
*&      Form  ALTERA_AUAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ANLA_OBJNR  text
*----------------------------------------------------------------------*
FORM altera_auai  USING    p_objnr
                           p_xafabe
                           p_yafabe.

  READ TABLE gt_auak WITH KEY objnr = p_objnr.
  IF sy-subrc IS INITIAL.
    LOOP AT gt_auai WHERE belnr = gt_auak-belnr
                      AND afabe = p_xafabe.
      gt_auai-afabe = p_yafabe.
      APPEND gt_auai.
      WRITE: / 'AUAI-registro criado:',
             gt_auak-objnr,
             gt_auai-belnr,
             gt_auai-afabe,
             gt_auai-anbtr.
      CLEAR gt_auai.
    ENDLOOP.
  ENDIF.
  CLEAR gt_auak.
ENDFORM.                    " ALTERA_AUAI
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_dados .

  MODIFY anlb FROM TABLE gt_anlb.
  MODIFY anlbza FROM TABLE gt_anlbza.
  MODIFY anlc FROM TABLE gt_anlc.
  MODIFY anep FROM TABLE gt_anep.
  MODIFY anea FROM TABLE gt_anea.
  MODIFY auai FROM TABLE gt_auai.

ENDFORM.                    " ATUALIZA_DADOS
