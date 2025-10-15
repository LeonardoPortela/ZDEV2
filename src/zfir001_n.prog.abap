*----------------------------------------------------------------------*
*                 B B K O     C O N S U L T I N G                      *
*----------------------------------------------------------------------*

REPORT  zfir001_n.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*

TYPE-POOLS: abap.

TABLES: anla,
        anlb,
        anlc,
        anep,
        anea,
        anli,
        auai,
        auak,
        coepbr.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*

TYPES: BEGIN OF y_upload,
         anln1  TYPE anla-anln1,
         anln2  TYPE anla-anln2,
         xafabe TYPE anlb-afabe,
         yafabe TYPE anlb-afabe,
         nafap  TYPE anlc-nafap,
       END OF y_upload.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*

DATA: tg_anla   TYPE anla     OCCURS 0 WITH HEADER LINE,
      tg_anlb   TYPE anlb     OCCURS 0 WITH HEADER LINE,
      tg_anlc   TYPE anlc     OCCURS 0 WITH HEADER LINE,
      tg_anep   TYPE anep     OCCURS 0 WITH HEADER LINE,
      tg_anea   TYPE anea     OCCURS 0 WITH HEADER LINE,
      tg_auak   TYPE auak     OCCURS 0 WITH HEADER LINE,
      tg_auai   TYPE auai     OCCURS 0 WITH HEADER LINE,
      tg_anlz   TYPE anlz     OCCURS 0 WITH HEADER LINE,
      tg_upload TYPE y_upload OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*

CONSTANTS: c_x       TYPE c VALUE 'X',
           c_0000(4) TYPE c VALUE '0000',
           c_zzz1(4) TYPE c VALUE 'ZZZ1'.

*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*

DATA: vg_erro1 TYPE c,
      vg_flag  TYPE c.

*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE scrtit.

SELECT-OPTIONS: xbukrs FOR anlb-bukrs.
SELECT-OPTIONS: xanln1 FOR anlb-anln1.
SELECT-OPTIONS: xanln2 FOR anlb-anln2.

PARAMETERS: xafabe  LIKE anlb-afabe. "OBLIGATORY.    "Source
PARAMETERS: yafabe  LIKE anlb-afabe, "OBLIGATORY.    "Target
            p_gjahr TYPE gjahr        OBLIGATORY.
PARAMETERS: xflag AS CHECKBOX DEFAULT abap_true,
            p_mov AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK 1.

SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE arq.

PARAMETERS: p_arq_ch AS CHECKBOX USER-COMMAND scr,
            p_arq    LIKE rlgrap-filename.

SELECTION-SCREEN END OF BLOCK 2.

*----------------------------------------------------------------------*
* At Selection-screen                                                  *
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

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*

INITIALIZATION.
  MOVE 'Abertura automática de uma nova área de avaliação' TO scrtit.

*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.

*Inicio da Alteração - TRRE
  IF p_arq_ch EQ 'X'.

    PERFORM carrega_arq.

    SELECT * FROM anla INTO TABLE tg_anla
      FOR ALL ENTRIES IN tg_upload
      WHERE bukrs IN xbukrs
        AND anln1 EQ tg_upload-anln1
        AND anln2 EQ tg_upload-anln2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i000(z01) WITH 'Não existem dados para esta seleção'.
      vg_erro1 = c_x.
    ENDIF.

  ELSE.

    IF xafabe IS INITIAL OR
       yafabe IS INITIAL.

      MESSAGE i000(z01) WITH 'Informar áreas de origem/destino.'.
      vg_erro1 = c_x.

    ELSE.

      SELECT * FROM anla INTO TABLE tg_anla
        WHERE bukrs IN xbukrs
          AND anln1 IN xanln1
          AND anln2 IN xanln2.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE i000(z01) WITH 'Não existem dados para esta seleção'.
        vg_erro1 = c_x.
      ENDIF.

    ENDIF.

  ENDIF.

  IF vg_erro1 IS INITIAL.

    SELECT * FROM anlb INTO TABLE tg_anlb
      FOR ALL ENTRIES IN tg_anla
      WHERE bukrs IN xbukrs
        AND anln1 EQ tg_anla-anln1
        AND anln2 EQ tg_anla-anln2.

    SELECT * FROM anlc INTO TABLE tg_anlc
      FOR ALL ENTRIES IN tg_anla
      WHERE bukrs EQ tg_anla-bukrs
        AND anln1 EQ tg_anla-anln1
        AND anln2 EQ tg_anla-anln2.

    IF p_mov IS INITIAL.

      SELECT * FROM anep INTO TABLE tg_anep
        FOR ALL ENTRIES IN tg_anla
          WHERE bukrs = tg_anla-bukrs
            AND anln1 = tg_anla-anln1
            AND anln2 = tg_anla-anln2.

    ENDIF.

    SELECT * FROM anea INTO TABLE tg_anea
      FOR ALL ENTRIES IN tg_anla
        WHERE bukrs = tg_anla-bukrs
          AND anln1 = tg_anla-anln1
          AND anln2 = tg_anla-anln2.

    SELECT * FROM anlz INTO TABLE tg_anlz
      FOR ALL ENTRIES IN tg_anla
      WHERE bukrs = tg_anla-bukrs
        AND anln1 = tg_anla-anln1
        AND anln2 = tg_anla-anln2.

    DELETE tg_anlz WHERE xstil EQ space.

    PERFORM trata_dados.

    IF NOT xflag = abap_true.
      PERFORM atualiza_dados.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO                                      *
*&---------------------------------------------------------------------*
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
*&      Form  CARREGA_ARQ                                              *
*&---------------------------------------------------------------------*
*       Carrega tabela com dados do arquivo                            *
*----------------------------------------------------------------------*
FORM carrega_arq .

  DATA: tl_aux TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_arq
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 5
      i_end_row               = 65536
    TABLES
      intern                  = tl_aux
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.

  ELSE.

    LOOP AT tl_aux.

      CASE tl_aux-col.

        WHEN 1.

          tg_upload-anln1 = tl_aux-value.
          SHIFT tg_upload-anln1 RIGHT DELETING TRAILING space.
          TRANSLATE tg_upload-anln1 USING ' 0' .

        WHEN 2.

          tg_upload-anln2 = tl_aux-value.
          SHIFT tg_upload-anln2 RIGHT DELETING TRAILING space.
          TRANSLATE tg_upload-anln2 USING ' 0' .

        WHEN 3.

          tg_upload-xafabe = tl_aux-value.
          SHIFT tg_upload-xafabe RIGHT DELETING TRAILING space.
          TRANSLATE tg_upload-xafabe USING ' 0' .

        WHEN 4.

          tg_upload-yafabe = tl_aux-value.
          SHIFT tg_upload-yafabe RIGHT DELETING TRAILING space.
          TRANSLATE tg_upload-yafabe USING ' 0' .

        WHEN 5.

          TRANSLATE tl_aux-value USING ',.' .
          tg_upload-nafap = tl_aux-value.

          APPEND tg_upload.
          CLEAR tg_upload.

      ENDCASE.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " CARREGA_ARQ
*&---------------------------------------------------------------------*
*&      Form  TRATA_DADOS                                              *
*&---------------------------------------------------------------------*
*       Trata dados                                                    *
*----------------------------------------------------------------------*
FORM trata_dados .

  DATA: vl_erro TYPE c.

  IF xflag = abap_true.
    WRITE: / '*** Teste! ***'.
  ELSE.
    WRITE: / '*** ATUALIZAÇÃO EFETIVA! ***'.
  ENDIF.

  IF p_arq_ch EQ 'X'."IS NOT INITIAL.

    LOOP AT tg_upload.

      READ TABLE tg_anla WITH KEY anln1 = tg_upload-anln1
                                  anln2 = tg_upload-anln2.
      IF sy-subrc IS INITIAL.

        PERFORM altera_anlb USING tg_upload-anln1  tg_upload-anln2
                                  tg_upload-xafabe tg_upload-yafabe
                            CHANGING vl_erro.

        IF vl_erro IS INITIAL.

          PERFORM altera_anlc USING  tg_upload-anln1  tg_upload-anln2
                                     tg_upload-xafabe tg_upload-yafabe
                                     tg_upload-nafap.

          IF NOT p_mov EQ 'X'."IS NOT INITIAL.

            PERFORM altera_anep USING tg_upload-anln1  tg_upload-anln2
                                      tg_upload-xafabe tg_upload-yafabe.
          ENDIF.

          IF NOT p_mov EQ 'X'."IS NOT INITIAL.

            PERFORM altera_anea USING tg_upload-anln1  tg_upload-anln2
                                      tg_upload-xafabe tg_upload-yafabe.
          ENDIF.

        ELSE.

          CLEAR vl_erro.
          CONTINUE.

        ENDIF.

      ELSE.

        CONTINUE.

      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT tg_anla.

      PERFORM altera_anlb USING tg_anla-anln1 tg_anla-anln2
                                xafabe yafabe
                       CHANGING vl_erro.

      IF vl_erro IS INITIAL.

        PERFORM altera_anlc USING  tg_anla-anln1 tg_anla-anln2
                                   xafabe yafabe space.
        IF p_mov IS INITIAL.

          PERFORM altera_anep USING tg_anla-anln1 tg_anla-anln2
                                    xafabe yafabe.
        ENDIF.

        IF p_mov IS INITIAL.

          PERFORM altera_anea USING tg_anla-anln1 tg_anla-anln2
                                    xafabe yafabe.
        ENDIF.
      ELSE.

        CLEAR vl_erro.
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
*      -->P_UPLOAD_ANLN1  text
*      -->P_UPLOAD_ANLN2  text
*      -->P_UPLOAD_XAFABE  text
*      -->P_UPLOAD_YAFABE  text
*      <--P_ERRO  text
*----------------------------------------------------------------------*
FORM altera_anlb  USING    p_anln1
                           p_anln2
                           p_xafabe
                           p_yafabe
                  CHANGING p_erro.

  READ TABLE tg_anlb WITH KEY anln1 = p_anln1
                              anln2 = p_anln2
                              afabe = p_yafabe.
*  IF sy-subrc IS INITIAL.
*
*    WRITE: / tg_anla-bukrs, tg_anla-anln1, tg_anla-anln2,
*             'Area já existe na tabela ANLB (não atualizar):',
*             tg_upload-yafabe.
*    p_erro = c_x.
*
*  ELSE.

  READ TABLE tg_anlb WITH KEY anln1 = p_anln1
                              anln2 = p_anln2
                              afabe = p_xafabe.

  IF sy-subrc IS INITIAL.

    tg_anlb-afabe = p_yafabe.

*      IF NOT p_mov IS INITIAL.
*        tg_anlb-afasl = c_0000.
*      ELSE.
*        tg_anlb-afasl = c_zzz1.
*      ENDIF.

    CLEAR: tg_anlb-aprop.

    APPEND tg_anlb.
    WRITE: / 'ANLB-registro criado:',
             tg_anlb-bukrs,
             tg_anlb-anln1,
             tg_anlb-anln2,
             tg_anlb-afabe,
             sy-uname.
    CLEAR tg_anlb.

  ENDIF.
*  ENDIF.

ENDFORM.                    " ALTERA_ANLB
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
                           p_yafabe
                           p_nafap.

  READ TABLE tg_anlz WITH KEY bukrs = tg_anla-bukrs
                              anln1 = tg_anla-anln1
                              anln2 = tg_anla-anln2.
  IF sy-subrc IS INITIAL.
    vg_flag = 'X'.
  ELSE.
    CLEAR vg_flag.
  ENDIF.

  LOOP AT tg_anlc WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.

    tg_anlc-afabe = p_yafabe.

    IF p_mov EQ 'X'.
      CLEAR: tg_anlc-kansw,
             tg_anlc-kaufw,
             tg_anlc-kinvz,
             tg_anlc-knafa,
             tg_anlc-ksafa,
             tg_anlc-kaafa,
             tg_anlc-kmafa,
             tg_anlc-kzinw,
             tg_anlc-kaufn,
             tg_anlc-kanza,
             tg_anlc-kvost,
             tg_anlc-aufwp,
             tg_anlc-nafap,
             tg_anlc-safap,
             tg_anlc-aafap,
             tg_anlc-mafap,
             tg_anlc-zinsp,
             tg_anlc-aufnp,
             tg_anlc-aufwb,
             tg_anlc-nafag,
             tg_anlc-safag,
             tg_anlc-aafag,
             tg_anlc-mafag,
             tg_anlc-zinsg,
             tg_anlc-aufng,
             tg_anlc-answl,
             tg_anlc-abgan,
             tg_anlc-ansaz,
             tg_anlc-aufwm,
             tg_anlc-invzm,
             tg_anlc-nafam,
             tg_anlc-safam,
             tg_anlc-aafam,
             tg_anlc-mafam,
             tg_anlc-zinsm,
             tg_anlc-aufnm,
             tg_anlc-zusna,
             tg_anlc-zussa,
             tg_anlc-zusaa,
             tg_anlc-zusma,
             tg_anlc-aufwv,
             tg_anlc-invzv,
             tg_anlc-nafav,
             tg_anlc-safav,
             tg_anlc-aafav,
             tg_anlc-mafav,
             tg_anlc-aufnv,
             tg_anlc-aufwl,
             tg_anlc-invzl,
             tg_anlc-nafal,
             tg_anlc-safal,
             tg_anlc-aafal,
             tg_anlc-mafal,
             tg_anlc-aufnl.
    ENDIF.

    IF tg_anlc-gjahr EQ p_gjahr AND
       vg_flag       NE 'X'.
      tg_anlc-nafap = p_nafap.
      tg_anlc-nafag = p_nafap.
    ENDIF.

    APPEND tg_anlc.

    WRITE: / 'ANLC-registro criado:',
           tg_anlc-bukrs,
           tg_anlc-anln1,
           tg_anlc-anln2,
           tg_anlc-afabe,
           tg_anlc-gjahr.

    CLEAR tg_anlc.

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

  LOOP AT tg_anep WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.

    tg_anep-afabe = p_yafabe.
    APPEND tg_anep.

    WRITE: / 'ANEP-registro criado:',
           tg_anep-bukrs,
           tg_anep-anln1,
           tg_anep-anln2,
           tg_anep-afabe,
           tg_anep-gjahr,
           tg_anep-belnr.

    CLEAR tg_anep.

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

  LOOP AT tg_anea WHERE anln1 = p_anln1
                    AND anln2 = p_anln2
                    AND afabe = p_xafabe.

    IF sy-subrc IS INITIAL.

      tg_anea-afabe = p_yafabe.
      APPEND tg_anea.

      WRITE: / 'ANEA-registro criado:',
             tg_anea-bukrs,
             tg_anea-anln1,
             tg_anea-anln2,
             tg_anea-afabe,
             tg_anea-gjahr,
             tg_anea-lnran.

      CLEAR tg_anea.

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

  READ TABLE tg_auak WITH KEY objnr = p_objnr.

  IF sy-subrc IS INITIAL.
    LOOP AT tg_auai WHERE belnr = tg_auak-belnr
                      AND afabe = p_xafabe.

      tg_auai-afabe = p_yafabe.
      APPEND tg_auai.

      WRITE: / 'AUAI-registro criado:',
             tg_auak-objnr,
             tg_auai-belnr,
             tg_auai-afabe,
             tg_auai-anbtr.
      CLEAR tg_auai.

    ENDLOOP.

  ENDIF.

  CLEAR tg_auak.

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

  MODIFY anlb FROM TABLE tg_anlb.
  MODIFY anlc FROM TABLE tg_anlc.
  MODIFY anep FROM TABLE tg_anep.
  MODIFY anea FROM TABLE tg_anea.
  MODIFY auai FROM TABLE tg_auai.

ENDFORM.                    " ATUALIZA_DADOS
