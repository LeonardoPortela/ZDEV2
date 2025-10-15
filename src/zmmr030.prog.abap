*&--------------------------------------------------------------------&*
*& Report Name    : Resumo do Beneficiamento.                         *&
*& Author         : Victor Hugo                                       *&
*& Date           : 17.05.2012                                        *&
*& Funcional Area : MM                                                *&
*&                                                                    *&
*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& Igor Sobral                  19.06.2013   Add - Funcao             &*
*&                                           Z_DADOSCLASSIFICACAOLOTE &*
*&--------------------------------------------------------------------&*
REPORT  zmmr030.
*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: chvw, s226, mch1, ausp, marc, mara.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPES:  BEGIN OF ty_zppt0001,
          werks     TYPE zppt0001-werks,
          matnr     TYPE zppt0001-matnr,
          tipo      TYPE zppt0001-tipo,
        END OF ty_zppt0001,

        BEGIN OF ty_s226,
          sptag     TYPE s226-sptag,
          werks     TYPE s226-werks,
          mcomp     TYPE s226-mcomp,
          enmng     TYPE s226-enmng,
          tipo      TYPE zppt0001-tipo,
        END OF ty_s226,

        BEGIN OF ty_mch1,
          matnr     TYPE mch1-matnr,
          charg     TYPE mch1-charg,
          cuobj_bm  TYPE mch1-cuobj_bm,
          objek     TYPE ausp-objek,
        END OF ty_mch1,

        BEGIN OF ty_mch1_count,
          matnr     TYPE mch1-matnr,
          charg     TYPE mch1-charg,
        END OF ty_mch1_count,

        BEGIN OF ty_ausp,
          objek     TYPE ausp-objek,
          atinn     TYPE ausp-atinn,
          atwrt     TYPE ausp-atwrt,
          atflv     TYPE ausp-atflv,
        END OF ty_ausp,

        BEGIN OF ty_mara,
          matnr     TYPE mara-matnr,
          matkl     TYPE mara-matkl,
        END OF ty_mara,

        BEGIN OF ty_marc,
          matnr     TYPE marc-matnr,
          werks     TYPE marc-werks,
        END OF ty_marc,

        BEGIN OF ty_consumo,
           werks    TYPE chvw-werks,
           budat    TYPE chvw-budat,
           total    TYPE chvw-menge,
           qtd      TYPE sy-tabix,
        END OF ty_consumo,

        BEGIN OF ty_produzido,
           werks    TYPE chvw-werks,
           budat    TYPE chvw-budat,
           total    TYPE chvw-menge,
        END OF ty_produzido,

        BEGIN OF ty_caroco,
          sptag     TYPE s226-sptag,
          werks     TYPE s226-werks,
          mcomp     TYPE s226-mcomp,
          enmng     TYPE s226-enmng,
          tipo      TYPE zppt0001-tipo,
        END OF ty_caroco,

        BEGIN OF ty_fibrilha,
          sptag     TYPE s226-sptag,
          werks     TYPE s226-werks,
          mcomp     TYPE s226-mcomp,
          enmng     TYPE s226-enmng,
          tipo      TYPE zppt0001-tipo,
        END OF ty_fibrilha,

        BEGIN OF ty_rendimento,
          budat     TYPE chvw-budat,
          rendi     TYPE s226-enmng,
        END OF ty_rendimento,

        BEGIN OF ty_fardos,
          budat     TYPE chvw-budat,
          charg     TYPE chvw-charg,
          produ     TYPE sy-tabix,
        END OF ty_fardos,

        BEGIN OF ty_fardos_produzidos,
          budat     TYPE chvw-budat,
          produ     TYPE sy-tabix,
        END OF ty_fardos_produzidos,

        BEGIN OF ty_principal,
          budat     TYPE chvw-budat,
          total_c   TYPE chvw-menge,
          qtd_c     TYPE sy-tabix,
          total_p   TYPE chvw-menge,
          caroco    TYPE s226-enmng,
          fibrilha  TYPE s226-enmng,
          rendi     TYPE s226-enmng,
          produ     TYPE sy-tabix,
        END OF ty_principal,

        BEGIN OF ty_saida,
          budat     TYPE chvw-budat,
          total_c   TYPE db20199vp,
          qtd_c     TYPE sy-tabix,
          total_p   TYPE db20199vp,
          caroco    TYPE db20199vp,
          fibrilha  TYPE db20199vp,
          rendi     TYPE p DECIMALS 2,
          produ     TYPE sy-tabix,
        END OF ty_saida,

        BEGIN OF ty_resumo,
          total_c   TYPE db20199vp,
          qtd_c     TYPE sy-tabix,
          total_p   TYPE db20199vp,
          caroco    TYPE db20199vp,
          fibrilha  TYPE db20199vp,
          rendi     TYPE p DECIMALS 2,
          produ     TYPE sy-tabix,
        END OF ty_resumo,

        BEGIN OF ty_cont,
          charg     TYPE chvw-charg,
          bwart     TYPE chvw-bwart,
        END OF ty_cont,

        BEGIN OF ty_zmmt0025,           "ADD - 19.06.2013
          atinn     TYPE zmmt0025-atinn,
          atnam     TYPE zmmt0025-atnam,
        END OF ty_zmmt0025.

*&--------------------------------------------------------------------&*
*& INTERNAL TABLE
*&--------------------------------------------------------------------&*
DATA: it_chvw               TYPE TABLE OF chvw WITH HEADER LINE,
      it_chvw_aux           TYPE TABLE OF chvw WITH HEADER LINE,
      it_mseg               TYPE TABLE OF mseg WITH HEADER LINE,

      it_zppt0001           TYPE TABLE OF ty_zppt0001,
      it_consumo            TYPE TABLE OF ty_consumo,
      it_produzido          TYPE TABLE OF ty_produzido,
      it_s226               TYPE TABLE OF ty_s226,
      it_s226_aux           TYPE TABLE OF ty_s226,

      it_mch1               TYPE TABLE OF ty_mch1,
      it_mch1_count         TYPE TABLE OF ty_mch1_count WITH HEADER LINE,
      it_mch1_aux           TYPE TABLE OF ty_mch1,
      it_ausp               TYPE TABLE OF ty_ausp,
      it_ausp_s             TYPE TABLE OF ty_ausp,
      it_ausp_t             TYPE TABLE OF ty_ausp,
      it_ausp_v             TYPE TABLE OF ty_ausp,
      it_mara               TYPE TABLE OF ty_mara,
      it_marc               TYPE TABLE OF ty_marc,

      it_caroco             TYPE TABLE OF ty_caroco,
      it_caroco_aux         TYPE TABLE OF ty_caroco,
      it_caroco_sum         TYPE TABLE OF ty_caroco,

      it_fibrilha           TYPE TABLE OF ty_fibrilha,
      it_fibrilha_aux       TYPE TABLE OF ty_fibrilha,
      it_fibrilha_sum       TYPE TABLE OF ty_fibrilha,

      it_rendimento         TYPE TABLE OF ty_rendimento,
      it_fardos             TYPE TABLE OF ty_fardos,
      it_fardos_aux         TYPE TABLE OF ty_fardos,
      it_fardos_produzidos  TYPE TABLE OF ty_fardos_produzidos,
      it_principal          TYPE TABLE OF ty_principal,
      it_principal_aux      TYPE TABLE OF ty_principal,
      it_saida              TYPE TABLE OF ty_saida,
      it_saida_aux          TYPE TABLE OF ty_saida,
      it_resumo             TYPE TABLE OF ty_resumo,

      it_matnr              TYPE TABLE OF zmme_cl,      "ADD - 19.06.2013
      it_return             TYPE TABLE OF zmme_cl,      "ADD - 19.06.2013
      it_return_aux         TYPE TABLE OF zmme_cl,      "ADD - 19.06.2013
      it_zmmt0025           TYPE TABLE OF ty_zmmt0025,  "ADD - 19.06.2013
      wa_return             TYPE zmme_cl,               "ADD - 19.06.2013
      wa_zmmt0025           TYPE ty_zmmt0025.           "ADD - 19.06.2013

*&--------------------------------------------------------------------&*
*& WORK AREA
*&--------------------------------------------------------------------&*
DATA: wa_chvw               TYPE chvw,
      wa_chvw_aux           TYPE chvw,
      wa_zppt0001           TYPE ty_zppt0001,
      wa_consumo            TYPE ty_consumo,
      wa_produzido          TYPE ty_produzido,
      wa_s226               TYPE ty_s226,
      wa_s226_aux           TYPE ty_s226,
      wa_mch1               TYPE ty_mch1,
      wa_mch1_aux           TYPE ty_mch1,
      wa_ausp               TYPE ty_ausp,
      wa_ausp_s             TYPE ty_ausp,
      wa_ausp_t             TYPE ty_ausp,
      wa_ausp_v             TYPE ty_ausp,
      wa_mara               TYPE ty_mara,
      wa_marc               TYPE ty_marc,
      wa_cont               TYPE ty_cont,

      wa_caroco             TYPE ty_caroco,
      wa_caroco_aux         TYPE ty_caroco,
      wa_caroco_sum         TYPE ty_caroco,

      wa_fibrilha           TYPE ty_fibrilha,
      wa_fibrilha_aux       TYPE ty_fibrilha,
      wa_fibrilha_sum       TYPE ty_fibrilha,

      wa_rendimento         TYPE ty_rendimento,
      wa_fardos             TYPE ty_fardos,
      wa_fardos_aux         TYPE ty_fardos,
      wa_fardos_produzidos  TYPE ty_fardos_produzidos,
      wa_principal          TYPE ty_principal,
      wa_principal_aux      TYPE ty_principal,
      wa_saida              TYPE ty_saida,
      wa_saida_aux          TYPE ty_saida,
      wa_resumo             TYPE ty_resumo.
*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
DATA: cl_container TYPE REF TO cl_gui_custom_container,
      cl_grid      TYPE REF TO cl_gui_alv_grid,
      it_fcat      TYPE lvc_t_fcat,
      wa_fcat      TYPE lvc_s_fcat,
      wa_layout    TYPE lvc_s_layo,
      wa_variant   TYPE disvariant.

*&--------------------------------------------------------------------&*
*& Condições
*&--------------------------------------------------------------------&*
DATA: it_atinn TYPE RANGE OF ausp-atwrt,
      wa_atinn LIKE LINE OF it_atinn.

*&--------------------------------------------------------------------&*
*& Textos
*&--------------------------------------------------------------------&*
CONSTANTS:  data(5)                  TYPE c VALUE 'Data',
            fardoes_beneficiados(50) TYPE c VALUE 'Fardões Beneficiados',
            peso_fardoes(50)         TYPE c VALUE 'Peso Fardões',
            fardos_produzidos(50)    TYPE c VALUE 'Fardos Produzidos',
            peso_fardos(50)          TYPE c VALUE 'Peso Fardos',
            caroco(8)                TYPE c VALUE 'Caroço',
            fibrilha(11)             TYPE c VALUE 'Fibrilha',
            rendimento(30)           TYPE c VALUE 'Rendimento %'.

*&--------------------------------------------------------------------&*
*& Parameters
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:  p_werks    FOR marc-werks NO INTERVALS NO-EXTENSION OBLIGATORY,
                 p_budat    FOR chvw-budat NO-EXTENSION OBLIGATORY,
                 p_matkl    FOR mara-matkl NO INTERVALS NO-EXTENSION OBLIGATORY,
                 p_lote     FOR chvw-charg NO INTERVALS NO-EXTENSION OBLIGATORY,
                 p_talhao   FOR ausp-atwrt NO INTERVALS NO-EXTENSION,
                 p_vari     FOR ausp-atwrt NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.


START-OF-SELECTION.
  PERFORM: seleciona_dados.
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.
  PERFORM: seleciona_consumo.

  CHECK NOT it_consumo[] IS INITIAL.
  PERFORM: seleciona_produzido,
           seleciona_subprodutos,
           calculo_rendimento,
           fardos_produzidos,
           display_alv.
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_CONSUMO
*&---------------------------------------------------------------------*
FORM seleciona_consumo.

  DATA: total     TYPE chvw-menge,
        safra_aux TYPE numc4,
        safra     TYPE ausp-atflv,
        qtd       TYPE sy-tabix,
        linhas    TYPE sy-tabix.

  CLEAR: it_mseg[].

  safra_aux = p_lote.
  safra = safra_aux.

  SELECT werks matnr tipo
    FROM zppt0001
    INTO TABLE it_zppt0001
  WHERE tipo EQ 'C'
    AND werks IN p_werks.

  CHECK NOT it_zppt0001[] IS INITIAL.

  SELECT * FROM chvw
    INTO TABLE it_chvw
    FOR ALL ENTRIES IN it_zppt0001
  WHERE matnr EQ it_zppt0001-matnr
    AND werks IN p_werks
*    AND budat IN p_budat
    AND budat BETWEEN p_budat-low AND p_budat-high
    AND bwart IN ('261','262').

  DELETE it_chvw WHERE xstbw EQ 'X'.
  DELETE it_chvw WHERE charg NA '/'.
  CHECK NOT it_chvw[] IS INITIAL.

  SELECT * FROM mseg
    INTO TABLE it_mseg
    FOR ALL ENTRIES IN it_chvw
   WHERE smbln EQ it_chvw-mblnr
     AND sjahr EQ it_chvw-mjahr
     AND smblp EQ it_chvw-zeile.

  LOOP AT it_mseg.
    READ TABLE it_chvw WITH KEY mblnr = it_mseg-smbln
                                mjahr = it_mseg-sjahr
                                zeile = it_mseg-smblp.
    IF sy-subrc IS INITIAL.
      DELETE it_chvw INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  CHECK NOT it_chvw[] IS INITIAL.

  SELECT matnr charg cuobj_bm
  FROM mch1
    INTO TABLE it_mch1_aux
    FOR ALL ENTRIES IN it_chvw
  WHERE matnr EQ it_chvw-matnr
    AND charg EQ it_chvw-charg.

  CHECK NOT it_mch1_aux[] IS INITIAL.

  CLEAR: it_mch1_count[].

  LOOP AT it_mch1_aux INTO wa_mch1_aux.
    wa_mch1-matnr = wa_mch1_aux-matnr.
    wa_mch1-charg = wa_mch1_aux-charg.
    wa_mch1-objek = wa_mch1_aux-cuobj_bm.
    APPEND wa_mch1 TO it_mch1.
    CLEAR: wa_mch1.
  ENDLOOP.

  CHECK NOT it_mch1[] IS INITIAL.

**COMMENT - 19.06.2013
**  SELECT objek atinn atwrt atflv
**  FROM ausp
**    INTO TABLE it_ausp_s
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atflv EQ safra.
**
**  SELECT objek atinn atwrt atflv
**  FROM ausp
**    INTO TABLE it_ausp_t
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atwrt IN p_talhao.
**
**  SELECT objek atinn atwrt atflv
**    FROM ausp
**    INTO TABLE it_ausp_v
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atwrt IN p_vari.
**
**  CHECK NOT it_ausp_s[] IS INITIAL.
**
**  LOOP AT it_ausp_s INTO wa_ausp_s.
**    READ TABLE it_ausp_t INTO wa_ausp_t WITH KEY objek = wa_ausp_s-objek.
**    IF ( sy-subrc EQ 0 ).
**      READ TABLE it_ausp_v INTO wa_ausp_v WITH KEY objek = wa_ausp_s-objek.
**      IF ( sy-subrc EQ 0 ).
**        wa_ausp-objek = wa_ausp_v-objek.
**        wa_ausp-atinn = wa_ausp_v-atinn.
**        wa_ausp-atwrt = wa_ausp_v-atwrt.
**        wa_ausp-atflv = wa_ausp_s-atflv.
**
**        APPEND wa_ausp TO it_ausp.
**      ENDIF.
**    ENDIF.
**
**    CLEAR: wa_ausp_s, wa_ausp_v, wa_ausp_t, wa_ausp.
**  ENDLOOP.

** ADD - 19.06.2013 - Inicio
  SELECT atinn atnam FROM zmmt0025 INTO TABLE it_zmmt0025.

  CHECK it_zmmt0025 IS NOT INITIAL.
  SORT it_zmmt0025 BY atnam.

  CLEAR:    wa_return.
  REFRESH:  it_matnr, it_return.

*  LOOP AT it_mseg.
*    MOVE: it_mseg-matnr     TO wa_return-matnr,
*          it_mseg-charg     TO wa_return-charg.
  LOOP AT it_chvw.
    MOVE: it_chvw-matnr     TO wa_return-matnr,
          it_chvw-charg     TO wa_return-charg.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'TALHAO' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'VARIEDADE' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.
  ENDLOOP.

  IF it_matnr IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        t_matnr  = it_matnr
        t_return = it_return
      EXCEPTIONS
        erro4    = 1.
*        OTHERS         = 2.
    IF sy-subrc <> 0.
*      sy-subrc = 1 " Dados não encontrados
    ELSE.
      IF p_lote-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_lote-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      IF p_talhao-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'TALHAO' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_talhao-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      IF p_vari-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'VARIEDADE' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_vari-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM it_return_aux COMPARING matnr charg.
      " Deletar com SAFRA diferente da informada
      IF it_return_aux[] IS NOT INITIAL.
        CLEAR: wa_return.
        LOOP AT it_return_aux INTO wa_return.
          DELETE it_return WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
          DELETE it_mseg   WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK it_return IS NOT INITIAL.
** ADD - 19.06.2013 - Fim

  it_chvw_aux[] = it_chvw[].

  CHECK NOT it_chvw_aux[] IS INITIAL.

  LOOP AT it_chvw_aux INTO wa_chvw_aux.
    CLEAR: total, wa_consumo.

    wa_consumo-total = 0.
    wa_consumo-qtd   = 0.

    LOOP AT it_chvw INTO wa_chvw WHERE budat EQ wa_chvw_aux-budat.
      READ TABLE it_mch1_count WITH KEY matnr = wa_chvw-matnr
                                        charg = wa_chvw-charg.
      IF NOT sy-subrc IS INITIAL.
        wa_consumo-qtd = wa_consumo-qtd + 1.
        it_mch1_count-matnr = wa_chvw-matnr.
        it_mch1_count-charg = wa_chvw-charg.

        APPEND it_mch1_count.
      ENDIF.
** COMMENT - 19.06.2013
**      READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_chvw-matnr
**                                               charg = wa_chvw-charg.
**
**      READ TABLE it_ausp INTO wa_ausp WITH KEY objek = wa_mch1-objek.
      READ TABLE it_return INTO wa_return WITH KEY matnr = wa_chvw-matnr
                                                   charg = wa_chvw-charg.
      IF ( sy-subrc EQ 0 ).
        wa_consumo-total = wa_consumo-total + wa_chvw-menge.
        CLEAR: wa_chvw, wa_mch1.
      ENDIF.
    ENDLOOP.

    wa_consumo-werks = wa_chvw_aux-werks.
    wa_consumo-budat = wa_chvw_aux-budat.

    APPEND wa_consumo TO it_consumo.

    DELETE it_chvw_aux WHERE budat EQ wa_chvw_aux-budat.

    CLEAR: wa_cont.
  ENDLOOP.

  DELETE it_consumo WHERE total EQ 0.

  CLEAR: linhas.
  DESCRIBE TABLE it_consumo LINES linhas.

  IF NOT ( it_consumo[] IS INITIAL ).
    LOOP AT it_consumo INTO wa_consumo.
      wa_principal-budat   = wa_consumo-budat.
      wa_principal-total_c = wa_consumo-total.
      wa_principal-qtd_c   = wa_consumo-qtd.

      APPEND wa_principal TO it_principal.
      CLEAR: wa_principal, wa_consumo.
    ENDLOOP.
  ENDIF.

  CLEAR: it_zppt0001, it_chvw_aux, it_mch1, it_mch1_aux, it_ausp_s, it_ausp_t, it_ausp_v.

ENDFORM.                    " SELECIONA_CONSUMO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_PRODUZIDO
*&---------------------------------------------------------------------*
FORM seleciona_produzido.

  CHECK NOT it_consumo[] IS INITIAL.

  CLEAR: it_mseg[].

  DATA: total     TYPE chvw-menge,
        safra_aux TYPE numc4,
        safra     TYPE ausp-atflv.

  safra_aux = p_lote.
  safra = safra_aux.

  SELECT werks matnr tipo
  FROM zppt0001
    INTO TABLE it_zppt0001
  WHERE tipo EQ 'P'
    AND werks IN p_werks.

  CHECK NOT it_zppt0001[] IS INITIAL.

  SELECT * FROM chvw
    INTO TABLE it_chvw
    FOR ALL ENTRIES IN it_zppt0001
  WHERE matnr EQ it_zppt0001-matnr
    AND werks IN p_werks
*    AND budat IN p_budat.
    AND budat BETWEEN p_budat-low AND p_budat-high.

  CHECK NOT it_chvw[] IS INITIAL.

  DELETE it_chvw WHERE xstbw EQ 'X'.
  DELETE it_chvw WHERE charg NA '/'.
  CHECK NOT it_chvw[] IS INITIAL.

  SELECT * FROM mseg
    INTO TABLE it_mseg
    FOR ALL ENTRIES IN it_chvw
   WHERE smbln EQ it_chvw-mblnr
     AND sjahr EQ it_chvw-mjahr
     AND smblp EQ it_chvw-zeile.

  LOOP AT it_mseg.
    READ TABLE it_chvw WITH KEY mblnr = it_mseg-smbln
                                mjahr = it_mseg-sjahr
                                zeile = it_mseg-smblp.
    IF sy-subrc IS INITIAL.
      DELETE it_chvw INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  CHECK NOT it_chvw[] IS INITIAL.

  SELECT matnr charg cuobj_bm
  FROM mch1
    INTO TABLE it_mch1_aux
    FOR ALL ENTRIES IN it_chvw
  WHERE matnr EQ it_chvw-matnr
    AND charg EQ it_chvw-charg.

  CHECK NOT it_mch1_aux[] IS INITIAL.

  LOOP AT it_mch1_aux INTO wa_mch1_aux.
    wa_mch1-matnr = wa_mch1_aux-matnr.
    wa_mch1-charg = wa_mch1_aux-charg.
    wa_mch1-objek = wa_mch1_aux-cuobj_bm.

    APPEND wa_mch1 TO it_mch1.
    CLEAR: wa_mch1.
  ENDLOOP.

  CHECK NOT it_mch1[] IS INITIAL.

**  SELECT objek atinn atwrt atflv
**  FROM ausp
**    INTO TABLE it_ausp_s
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atwrt EQ safra.
***    AND atflv EQ safra.
**
**  SELECT objek atinn atwrt atflv
**  FROM ausp
**    INTO TABLE it_ausp_t
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atwrt IN p_talhao.
**
**  SELECT objek atinn atwrt atflv
**  FROM ausp
**    INTO TABLE it_ausp_v
**    FOR ALL ENTRIES IN it_mch1
**  WHERE objek EQ it_mch1-objek
**    AND atwrt IN p_vari.
**
**  CHECK NOT it_ausp_s[] IS INITIAL.
**
**  LOOP AT it_ausp_s INTO wa_ausp_s.
**    READ TABLE it_ausp_t INTO wa_ausp_t WITH KEY objek = wa_ausp_s-objek.
**    IF ( sy-subrc EQ 0 ).
**      READ TABLE it_ausp_v INTO wa_ausp_v WITH KEY objek = wa_ausp_s-objek.
**      IF ( sy-subrc EQ 0 ).
**        wa_ausp-objek = wa_ausp_v-objek.
**        wa_ausp-atinn = wa_ausp_v-atinn.
**        wa_ausp-atwrt = wa_ausp_v-atwrt.
**        wa_ausp-atflv = wa_ausp_s-atflv.
**
**        APPEND wa_ausp TO it_ausp.
**      ENDIF.
**    ENDIF.
**
**    CLEAR: wa_ausp_s, wa_ausp_v, wa_ausp_t, wa_ausp.
**  ENDLOOP.

** ADD - 19.06.2013 - Inicio
**  SELECT atinn atnam FROM zmmt0025 INTO TABLE it_zmmt0025.
**
**  CHECK it_zmmt0025 IS NOT INITIAL.
**  SORT it_zmmt0025 BY atnam.

  CLEAR:    wa_return.
  REFRESH:  it_matnr, it_return.

*  LOOP AT it_mseg.
*    MOVE: it_mseg-matnr     TO wa_return-matnr,
*          it_mseg-charg     TO wa_return-charg.
  LOOP AT it_chvw.
    MOVE: it_chvw-matnr     TO wa_return-matnr,
          it_chvw-charg     TO wa_return-charg.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'TALHAO' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.

    READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'VARIEDADE' BINARY SEARCH.
    MOVE  wa_zmmt0025-atinn TO wa_return-atinn.
    APPEND wa_return TO it_matnr.
  ENDLOOP.

  IF it_matnr IS NOT INITIAL.
    CALL FUNCTION 'Z_DADOSCLASSIFICACAOLOTE'
      TABLES
        t_matnr  = it_matnr
        t_return = it_return
      EXCEPTIONS
        erro4    = 1.
*        OTHERS         = 2.
    IF sy-subrc <> 0.
*      sy-subrc = 1 " Dados não encontrados
    ELSE.
      IF p_lote-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'SAFRA' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_lote-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      IF p_talhao-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'TALHAO' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_talhao-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      IF p_vari-low IS NOT INITIAL.
        CLEAR: wa_zmmt0025.
        READ TABLE it_zmmt0025 INTO wa_zmmt0025 WITH KEY atnam = 'VARIEDADE' BINARY SEARCH.
        LOOP AT it_return INTO wa_return WHERE atinn EQ wa_zmmt0025-atinn
                                           AND atwrt NE p_vari-low.
          APPEND wa_return TO it_return_aux.
        ENDLOOP.
      ENDIF.

      DELETE ADJACENT DUPLICATES FROM it_return_aux COMPARING matnr charg.
      "Deletar com SAFRA diferente da informada
      IF it_return_aux[] IS NOT INITIAL.
        CLEAR: wa_return.
        LOOP AT it_return_aux INTO wa_return.
          DELETE it_return WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
          DELETE it_mseg   WHERE matnr EQ wa_return-matnr AND charg EQ wa_return-charg.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

  CHECK it_return IS NOT INITIAL.
** ADD - 19.06.2013 - Fim

  it_chvw_aux[] = it_chvw[].

  CHECK NOT it_chvw_aux[] IS INITIAL.

  LOOP AT it_chvw_aux INTO wa_chvw_aux.
    CLEAR: total.

    LOOP AT it_chvw INTO wa_chvw WHERE budat EQ wa_chvw_aux-budat.
** COMMENT - 19.06.2013
**      READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = wa_chvw-matnr
**                                               charg = wa_chvw-charg.
**
**      READ TABLE it_ausp INTO wa_ausp WITH KEY objek = wa_mch1-objek.
      READ TABLE it_return INTO wa_return WITH KEY matnr = wa_chvw-matnr
                                                   charg = wa_chvw-charg.
      IF ( sy-subrc EQ 0 ).
        CASE wa_chvw-bwart.
          WHEN: '131'.
            total = total + wa_chvw-menge.
          WHEN: '132'.
            total = total - wa_chvw-menge.
        ENDCASE.

        CLEAR: wa_chvw.
      ENDIF.
    ENDLOOP.

    wa_produzido-werks = wa_chvw_aux-werks.
    wa_produzido-budat = wa_chvw_aux-budat.
    wa_produzido-total = total.

    APPEND wa_produzido TO it_produzido.

    DELETE it_chvw_aux WHERE budat EQ wa_chvw_aux-budat.
  ENDLOOP.

  DELETE it_produzido WHERE total EQ 0.

  IF NOT ( it_produzido[] IS INITIAL ).
    LOOP AT it_produzido INTO wa_produzido.
      wa_principal-budat   = wa_produzido-budat.
      wa_principal-total_p = wa_produzido-total.

      APPEND wa_principal TO it_principal.

      CLEAR: wa_principal, wa_produzido.
    ENDLOOP.
  ENDIF.

  CLEAR: it_zppt0001, it_chvw_aux, it_mch1, it_mch1_aux, it_ausp_s, it_ausp_t, it_ausp_v.
ENDFORM.                    " SELECIONA_PRODUZIDO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_SUBPRODUTOS
*&---------------------------------------------------------------------*
FORM seleciona_subprodutos.

  DATA: total   TYPE s226-enmng,
        index   TYPE sy-tabix,
        total_f TYPE s226-enmng,
        total_c TYPE s226-enmng.

  SELECT werks matnr tipo
  FROM zppt0001
    INTO TABLE it_zppt0001
  WHERE tipo IN ('S1','S2')
    AND werks IN p_werks.

  CHECK NOT it_zppt0001[] IS INITIAL.

  SELECT sptag werks mcomp enmng
  FROM s226
    INTO TABLE it_s226
    FOR ALL ENTRIES IN it_zppt0001
  WHERE mcomp EQ it_zppt0001-matnr
    AND sptag IN p_budat
    AND werks IN p_werks.

  CHECK NOT it_s226[] IS INITIAL.

  LOOP AT it_s226 INTO wa_s226.
    READ TABLE it_zppt0001 INTO wa_zppt0001 WITH KEY matnr = wa_s226-mcomp.
    IF ( sy-subrc EQ 0 ).
      CASE wa_zppt0001-tipo.
        WHEN: 'S1'.
          wa_fibrilha_aux-sptag =  wa_s226-sptag.
          wa_fibrilha_aux-werks =  wa_s226-werks.
          wa_fibrilha_aux-mcomp =  wa_s226-mcomp.
          wa_fibrilha_aux-enmng =  ( wa_s226-enmng * -1 ).
          wa_fibrilha_aux-tipo  =  wa_zppt0001-tipo.

          APPEND wa_fibrilha_aux TO it_fibrilha_aux.

          CLEAR: wa_fibrilha_aux.
        WHEN: 'S2'.
          wa_caroco_aux-sptag =  wa_s226-sptag.
          wa_caroco_aux-werks =  wa_s226-werks.
          wa_caroco_aux-mcomp =  wa_s226-mcomp.
          wa_caroco_aux-enmng =  ( wa_s226-enmng * -1 ).
          wa_caroco_aux-tipo  =  wa_zppt0001-tipo.

          APPEND wa_caroco_aux TO it_caroco_aux.

          CLEAR: wa_caroco_aux.
      ENDCASE.
    ENDIF.

    CLEAR: wa_s226, wa_zppt0001.
  ENDLOOP.

  IF NOT ( it_fibrilha_aux[] IS INITIAL ).
    it_fibrilha_sum[] = it_fibrilha_aux[].

    LOOP AT it_fibrilha_aux INTO wa_fibrilha_aux.
      CLEAR: total_f.

      LOOP AT it_fibrilha_sum INTO wa_fibrilha_sum WHERE sptag EQ wa_fibrilha_aux-sptag.
        total_f = total_f  + wa_fibrilha_sum-enmng.

        CLEAR: wa_fibrilha_sum.
      ENDLOOP.

      wa_fibrilha-sptag = wa_fibrilha_aux-sptag.
      wa_fibrilha-werks = wa_fibrilha_aux-werks.
      wa_fibrilha-mcomp = wa_fibrilha_aux-mcomp.
      wa_fibrilha-enmng = total_f.
      wa_fibrilha-tipo  = wa_fibrilha_aux-tipo.

      wa_principal-budat    = wa_fibrilha_aux-sptag.
      wa_principal-fibrilha = total_f.

      APPEND wa_principal TO it_principal.

      APPEND wa_fibrilha TO it_fibrilha.

      DELETE it_fibrilha_aux WHERE sptag = wa_fibrilha_aux-sptag.
      CLEAR: wa_fibrilha, wa_fibrilha_aux, wa_principal.
    ENDLOOP.
  ENDIF.

  IF NOT ( it_caroco_aux[] IS INITIAL ).
    it_caroco_sum[] = it_caroco_aux[].

    LOOP AT it_caroco_aux INTO wa_caroco_aux.
      CLEAR: total_c.

      LOOP AT it_caroco_sum INTO wa_caroco_sum WHERE sptag = wa_caroco_aux-sptag.
        total_c = total_c + wa_caroco_sum-enmng.

        CLEAR: wa_caroco_sum.
      ENDLOOP.

      wa_caroco-sptag = wa_caroco_aux-sptag.
      wa_caroco-werks = wa_caroco_aux-werks.
      wa_caroco-mcomp = wa_caroco_aux-mcomp.
      wa_caroco-enmng = total_c.
      wa_caroco-tipo  = wa_caroco_aux-tipo.

      wa_principal-budat    = wa_caroco_aux-sptag.
      wa_principal-caroco   = total_c.

      APPEND wa_caroco TO it_caroco.

      APPEND wa_principal TO it_principal.

      DELETE it_caroco_aux WHERE sptag EQ wa_caroco_aux-sptag.

      CLEAR: wa_caroco, wa_principal.
    ENDLOOP.
  ENDIF.

  CLEAR: it_zppt0001, it_s226, it_s226_aux.
ENDFORM.                    " SELECIONA_SUBPRODUTOS

*&---------------------------------------------------------------------*
*&      Form  CALCULO_RENDIMENTO
*&---------------------------------------------------------------------*
FORM calculo_rendimento .

  DATA: rendi TYPE s226-enmng.

  CLEAR: wa_produzido, wa_consumo.

  CHECK NOT it_produzido[] IS INITIAL.

  LOOP AT it_produzido INTO wa_produzido.
    LOOP AT it_consumo INTO wa_consumo WHERE budat EQ wa_produzido-budat.
      rendi = ( ( wa_produzido-total / wa_consumo-total ) * 100 ).

      CLEAR: wa_consumo.
    ENDLOOP.

    wa_rendimento-rendi = rendi.
    wa_rendimento-budat = wa_produzido-budat.

    APPEND wa_rendimento TO it_rendimento.

    CLEAR: rendi, wa_rendimento, wa_produzido.
  ENDLOOP.

  IF NOT ( it_rendimento[] IS INITIAL ).
    LOOP AT it_rendimento INTO wa_rendimento.
      wa_principal-budat = wa_rendimento-budat.
      wa_principal-rendi = wa_rendimento-rendi.

      APPEND wa_principal TO it_principal.

      CLEAR: wa_principal, wa_rendimento.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CALCULO_RENDIMENTO

*&---------------------------------------------------------------------*
*&      Form  FARDOS_PRODUZIDOS
*&---------------------------------------------------------------------*
FORM fardos_produzidos.
  DATA: qtd   TYPE sy-tabix,
        produ TYPE sy-tabix.

  CLEAR: it_mseg[].

  SELECT matnr matkl
  FROM mara
    INTO TABLE it_mara
  WHERE matkl IN p_matkl.

  CHECK NOT it_mara[] IS INITIAL.

  SELECT matnr werks
  FROM marc
    INTO TABLE it_marc
    FOR ALL ENTRIES IN it_mara
  WHERE matnr EQ it_mara-matnr
    AND werks IN p_werks.

  CHECK NOT it_marc[] IS INITIAL.

  SELECT * FROM chvw
    INTO TABLE it_chvw
    FOR ALL ENTRIES IN it_marc
  WHERE matnr EQ it_marc-matnr
    AND werks IN p_werks
    AND budat IN p_budat
    AND menge > 0
    AND bwart IN ('131','132').

  CHECK NOT it_chvw[] IS INITIAL.

  "delete it_chvw where xstbw eq 'X'.
  DELETE it_chvw WHERE charg NA '/'.
  CHECK NOT it_chvw[] IS INITIAL.

  SELECT * FROM mseg
    INTO TABLE it_mseg
    FOR ALL ENTRIES IN it_chvw
   WHERE smbln EQ it_chvw-mblnr
     AND sjahr EQ it_chvw-mjahr
     AND smblp EQ it_chvw-zeile.

  LOOP AT it_mseg.
    READ TABLE it_chvw WITH KEY mblnr = it_mseg-smbln
                                mjahr = it_mseg-sjahr
                                zeile = it_mseg-smblp.
    IF sy-subrc IS INITIAL.
      DELETE it_chvw INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  it_chvw_aux[] = it_chvw[].

  LOOP AT it_chvw INTO wa_chvw.
    LOOP AT it_chvw_aux INTO wa_chvw_aux WHERE budat EQ wa_chvw-budat.
      CASE wa_chvw_aux-bwart.
        WHEN: '131'.
          qtd = qtd + 1.
        WHEN: '132'.
          qtd = qtd - 1.
      ENDCASE.

      CLEAR: wa_chvw_aux.
    ENDLOOP.

    wa_fardos-produ = qtd.
    wa_fardos-budat = wa_chvw-budat.
    wa_fardos-charg = wa_chvw-charg.

    APPEND wa_fardos TO it_fardos.

    DELETE it_chvw WHERE budat = wa_chvw-budat.

    CLEAR: wa_chvw, wa_fardos, qtd.
  ENDLOOP.

  IF NOT ( it_fardos IS INITIAL ).
    LOOP AT it_fardos INTO wa_fardos.
      wa_principal-budat = wa_fardos-budat.
      wa_principal-produ = wa_fardos-produ.

      APPEND wa_principal TO it_principal.

      CLEAR: wa_principal, wa_fardos.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " FARDOS_PRODUZIDOS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: qtd TYPE sy-tabix,
        rendi TYPE s226-enmng.

  it_principal_aux[] = it_principal[].

  CLEAR: wa_principal, wa_saida.

  LOOP AT it_principal_aux INTO wa_principal_aux.
    LOOP AT it_principal INTO wa_principal WHERE budat = wa_principal_aux-budat.
      wa_saida-budat      = wa_principal-budat.

      IF ( wa_principal-total_c NE 0 ) .
        wa_saida-total_c  = wa_principal-total_c.
      ENDIF.

      IF ( wa_principal-qtd_c NE 0 ).
        wa_saida-qtd_c    = wa_principal-qtd_c.
      ENDIF.

      IF ( wa_principal-total_p NE 0 ).
        wa_saida-total_p  = wa_principal-total_p.
      ENDIF.

      IF ( wa_principal-caroco NE 0 ).
        wa_saida-caroco   = wa_principal-caroco.
      ENDIF.

      IF ( wa_principal-fibrilha NE 0 ).
        wa_saida-fibrilha = wa_principal-fibrilha.
      ENDIF.
      IF ( wa_principal-rendi NE 0 ).
        wa_saida-rendi    = wa_principal-rendi.
      ENDIF.

      wa_saida-produ      = wa_principal-produ.

      CLEAR: wa_principal.
    ENDLOOP.

    APPEND wa_saida TO it_saida.

    DELETE it_principal_aux WHERE budat = wa_principal_aux-budat.
    CLEAR: wa_saida.
  ENDLOOP.

  CLEAR: wa_resumo.

  SORT it_saida BY budat ASCENDING.

  LOOP AT it_saida INTO wa_saida.
    wa_resumo-total_c  = wa_resumo-total_c  + wa_saida-total_c.
    wa_resumo-qtd_c    = wa_resumo-qtd_c    + wa_saida-qtd_c.
    wa_resumo-total_p  = wa_resumo-total_p  + wa_saida-total_p.
    wa_resumo-caroco   = wa_resumo-caroco   + wa_saida-caroco.
    wa_resumo-fibrilha = wa_resumo-fibrilha + wa_saida-fibrilha.
    wa_resumo-produ    = wa_resumo-produ    + wa_saida-produ.

    rendi    = rendi + wa_saida-rendi.

    qtd = qtd + 1.
  ENDLOOP.

  "wa_resumo-rendi = ( rendi / qtd ).
  wa_resumo-rendi = ( wa_resumo-total_p / wa_resumo-total_c ) * 100.

  CLEAR: qtd.
ENDFORM.                    " DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  IF ( cl_container IS INITIAL ).
    PERFORM: init_container.
  ENDIF.

  SET PF-STATUS 'PF100'.
  SET TITLEBAR  'TB100'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT

*&---------------------------------------------------------------------*
*&      Form  INIT_CONTAINER
*&---------------------------------------------------------------------*
FORM init_container.
  CREATE OBJECT cl_container
    EXPORTING
      container_name              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  CREATE OBJECT cl_grid
    EXPORTING
      i_parent = cl_container.

  PERFORM: fcat.

  CALL METHOD cl_grid->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = wa_variant
      i_save                        = 'A'
    CHANGING
      it_outtab                     = it_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.                    " INIT_CONTAINER

*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
FORM fcat.

  PERFORM catalog USING:
    'BUDAT'     data                  '10' '' '' 'C500' '' '',
    'QTD_C'     fardoes_beneficiados  '20' '' '' ''     '' '',
    'TOTAL_C'   peso_fardoes          '12' '' '' ''     '' '',
    'PRODU'     fardos_produzidos     '17' '' '' ''     '' '',
    'TOTAL_P'   peso_fardos           '11' '' '' ''     '' '',
    'CAROCO'    caroco                '10' '' '' ''     '' '',
    'FIBRILHA'  fibrilha              '10' '' '' ''     '' '',
    'RENDI'     rendimento            '12' '' '' ''     '' ''.

ENDFORM.                    " FCAT

*&---------------------------------------------------------------------*
*&      Form  CATALOG
*&---------------------------------------------------------------------*
FORM catalog   USING    value(p_fieldname)
                        value(p_desc)
                        value(p_tam)
                        value(p_no_zero)
                        value(p_hotspot)
                        value(p_cor)
                        value(p_just)
                        value(p_sum).
  CLEAR: wa_fcat.

  wa_fcat-fieldname = p_fieldname.
  wa_fcat-scrtext_l = p_desc.
  wa_fcat-scrtext_m = p_desc.
  wa_fcat-scrtext_s = p_desc.
  wa_fcat-outputlen = p_tam.
  wa_fcat-no_zero   = p_no_zero.
  wa_fcat-hotspot   = p_hotspot.
  wa_fcat-emphasize = p_cor.
  wa_fcat-just      = p_just.
  wa_fcat-do_sum    = p_sum.

  APPEND wa_fcat TO it_fcat.
ENDFORM.                    " CATALOG
