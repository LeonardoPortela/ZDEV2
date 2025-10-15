*&--------------------------------------------------------------------&*
*&                        FI                                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 31/10/2014                                              &*
*& Descrição: Relatório Kardex                                        &*
*& Transação: ZMMR0007                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  zmmr0007 MESSAGE-ID z_mm.

TYPE-POOLS vrm.
*=============================================================================*
*TABELAS                                                                      *
*=============================================================================*

TABLES: j_1bbranch, mara, bsis.

*=============================================================================*
*Estrutura                                                                    *
*=============================================================================*

TYPES: BEGIN OF ty_saida,
         j_1bbranch     TYPE t001w-j_1bbranch,
         name1          TYPE t001w-name1,
         werks          TYPE mseg-werks,
         valor1         TYPE dmbtr,
         quant1         TYPE mseg-menge,
         valor2         TYPE dmbtr,
         quant2         TYPE mseg-menge,
         valor3         TYPE dmbtr,
         quant3         TYPE mseg-menge,
         valor4         TYPE dmbtr,
         quant4         TYPE mseg-menge,
         valor5         TYPE dmbtr,
         quant5         TYPE mseg-menge,
         valor6         TYPE dmbtr,
         quant6         TYPE mseg-menge,
         valor7         TYPE dmbtr,
         quant7         TYPE mseg-menge,
         valor8         TYPE dmbtr,
         quant8         TYPE mseg-menge,
         valor9         TYPE dmbtr,
         quant9         TYPE mseg-menge,
         valor10        TYPE dmbtr,
         quant10        TYPE mseg-menge,
         valor11        TYPE dmbtr,
         quant11        TYPE mseg-menge,
         valor12        TYPE dmbtr,
         quant12        TYPE mseg-menge,
         valor13        TYPE dmbtr,
         quant13        TYPE mseg-menge,
         valor14        TYPE dmbtr,
         quant14        TYPE mseg-menge,
         valor15        TYPE dmbtr,
         quant15        TYPE mseg-menge,
         valor16        TYPE dmbtr,
         quant16        TYPE mseg-menge,
         valor17        TYPE dmbtr,
         quant17        TYPE mseg-menge,
         valor18        TYPE dmbtr,
         quant18        TYPE mseg-menge,
         descr_abertura TYPE zmmt0060-descr_abertura,
         quant          TYPE mseg-menge,
         tipo           TYPE zmmt0060-tipo,
       END OF ty_saida.

TYPES: BEGIN OF ty_totais,
         totais         TYPE c,
         numero         TYPE i,
         valor          TYPE dmbtr,
         quant          TYPE mseg-menge,
         descr_abertura TYPE zmmt0060-descr_abertura,
         tipo           TYPE zmmt0060-tipo,
         werks          TYPE mseg-werks,
         j_1bbranch     TYPE t001w-j_1bbranch,
       END OF ty_totais.

TYPES: BEGIN OF ty_bsis.
         INCLUDE TYPE bsis.
TYPES:   mblnr LIKE mseg-mblnr,
         zeile TYPE	mblpo.
TYPES: END OF ty_bsis.

TYPES: BEGIN OF ty_resumo_alv,
         icone   TYPE icon-id,
         empresa TYPE s_bukrs,


       END OF ty_resumo_alv.

*=============================================================================*
*TABELA INTERNA                                                               *
*=============================================================================*

DATA: it_ckmlcr     TYPE TABLE OF ckmlcr,
      it_ckmlpp     TYPE TABLE OF ckmlpp,
      it_mbew       TYPE TABLE OF mbew,
      it_t030       TYPE TABLE OF t030,
      it_bsis       TYPE TABLE OF ty_bsis,
      it_bkpf       TYPE TABLE OF bkpf,
      it_mseg       TYPE TABLE OF mseg,
      it_zmmt0060   TYPE TABLE OF zmmt0060,
      it_t001w      TYPE TABLE OF t001w,
      it_makt       TYPE TABLE OF makt,
      it_ckmlhd     TYPE TABLE OF ckmlhd,
      it_totais     TYPE TABLE OF ty_totais,
      it_resumo_alv TYPE TABLE OF ty_resumo_alv,
      it_saida      TYPE TABLE OF ty_saida.


*=============================================================================*
*WORK AREA                                                                    *
*=============================================================================*

DATA: wa_skat       TYPE skat,
      wa_faglflext  TYPE faglflext,
      wa_ckmlcr     TYPE ckmlcr,
      wa_ckmlpp     TYPE ckmlpp,
      wa_mbew       TYPE mbew,
      wa_t030       TYPE t030,
      wa_bsis       TYPE ty_bsis,
      wa_bkpf       TYPE bkpf,
      wa_mseg       TYPE mseg,
      wa_zmmt0060   TYPE zmmt0060,
      wa_t001w      TYPE t001w,
      wa_makt       TYPE makt,
      wa_ckmlhd     TYPE ckmlhd,
      wa_saida      TYPE ty_saida,
      wa_cont       TYPE REF TO cl_gui_custom_container,
      wa_alv        TYPE REF TO  cl_gui_alv_grid,
      wa_layout     TYPE lvc_s_layo,
      wa_resumo_alv LIKE LINE OF it_resumo_alv,
      wa_totais     TYPE ty_totais.


*----------------------------------------------------------------------*
***INCLUDE Zmmr0007_0001 .
*----------------------------------------------------------------------*

DATA: dg_dyndoc_id     TYPE REF TO cl_dd_document.

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_xstr TYPE xstring.
DATA: graphic_size   TYPE i.
DATA: l_graphic_conv TYPE i.
DATA: l_graphic_offs TYPE i.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.
*    METHODS TOP_OF_PAGE
*      FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
*      IMPORTING E_DYNDOC_ID.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Inclementação  -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
*    PERFORM HANDLE_HOTSPOT_CLICK
*       USING ES_ROW_NO-ROW_ID
*             E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click
*  METHOD TOP_OF_PAGE.
*    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID.
*  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*=============================================================================*
*Estrutura cabeçalho Alv                                                      *
*=============================================================================*
DATA: picture          TYPE REF TO cl_gui_picture,
      gf_first_display TYPE c VALUE 'X',
      ctl_cccontainer  TYPE REF TO cl_gui_custom_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_html   TYPE REF TO cl_gui_container,
      dg_parent_html1  TYPE REF TO cl_gui_container,
      dg_parent_html2  TYPE REF TO cl_gui_container,
      dg_parent_grid   TYPE REF TO cl_gui_container,
      event_handler    TYPE REF TO lcl_event_handler,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      ctl_alv_resumo   TYPE REF TO cl_gui_alv_grid,
      gs_scroll_col    TYPE lvc_s_col,
      gs_scroll_row    TYPE lvc_s_roid,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_exclude_fcode TYPE ui_functions.



*=============================================================================*
*Estrutura Alv                                                                *
*=============================================================================*
DATA:it_fcat    TYPE TABLE OF lvc_s_fcat.
DATA:it_list    TYPE vrm_values,
     list_value TYPE vrm_values.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN:  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.


  SELECT-OPTIONS: s_bukrs FOR j_1bbranch-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY  ,
                  s_werks FOR j_1bbranch-branch NO-EXTENSION,
                  s_gjahr FOR bsis-gjahr NO-EXTENSION NO INTERVALS OBLIGATORY ,
                  s_perio FOR bsis-monat NO-EXTENSION OBLIGATORY,
                  s_matnr FOR mara-matnr NO INTERVALS NO-EXTENSION OBLIGATORY  .

  PARAMETERS: p_waers TYPE ckmlcr-waers OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.


*=============================================================================*
*Start-Of-Selection                                                           *
*=============================================================================*
START-OF-SELECTION.

  PERFORM f_seleciona_dados. " Form selecionar dado
  PERFORM f_organizar_dados.      " ORGANIZAR DADOS
  CALL SCREEN 0100.

*=============================================================================*
*Form F_SELECIONA_DADOS                                                       *
*=============================================================================*
FORM f_seleciona_dados.
  DATA: lv_xsdo_r  TYPE dmbtr,
        lv_xsdo_us TYPE dmbtr.

  SELECT *
   FROM ckmlhd
   INTO TABLE it_ckmlhd
    WHERE matnr IN s_matnr
      AND bwkey IN s_werks.

  IF ( it_ckmlhd IS INITIAL ).
    MESSAGE s000 WITH 'Não econtrado Ledger de material' 'para este centro' s_werks-low.
    STOP.
  ELSE.

    CASE p_waers.
      WHEN 'BRL'.
        "Moeda Empresa
* ---> S4 Migration - 08/07/2023 - MA
*        SELECT *
*          FROM ckmlcr
*          INTO TABLE it_ckmlcr
*           FOR ALL ENTRIES IN it_ckmlhd
*         WHERE curtp EQ '10'
*           AND kalnr EQ it_ckmlhd-kalnr
*           AND bdatj IN s_gjahr
*           AND poper IN s_perio.

        DATA: wa_kalnr  TYPE ckmv0_matobj_str,
              lt_kalnr  TYPE ckmv0_matobj_tbl,
              lt_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.

        DATA: lv_bdatj_1 TYPE  ckmlpp-bdatj,
              lv_poper_1 TYPE  ckmlpp-poper,
              lv_jahrper TYPE mldoc-jahrper.

        LOOP AT it_ckmlhd INTO DATA(wa_ckmlhd).
          wa_kalnr-kalnr = wa_ckmlhd-kalnr.
          APPEND wa_kalnr TO lt_kalnr.
        ENDLOOP.

        CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
*        EXPORTING
*          i_bdatj_1               = lv_bdatj_1
*          i_poper_1               = lv_poper_1
          TABLES
            t_kalnr                 = lt_kalnr
*           t_ckmlpp                = lt_ckmlpp
            t_ckmlcr                = lt_ckmlcr
          EXCEPTIONS
            no_data_found           = 1
            input_data_inconsistent = 2
            buffer_inconsistent     = 3
            OTHERS                  = 4.

        IF lt_ckmlcr[] IS NOT INITIAL.

          DELETE lt_ckmlcr WHERE  bdatj NOT IN s_gjahr  AND poper NOT IN s_perio AND curtp NE '10'.

          IF sy-subrc = 0 AND lines( lt_ckmlcr[] ) > 0.

            MOVE-CORRESPONDING lt_ckmlcr[] TO it_ckmlcr[].
            sy-dbcnt = lines( lt_ckmlcr[] ).
          ELSE.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ENDIF.
        ENDIF.
* <--- S4 Migration - 08/07/2023 - MA
      WHEN 'USD'.
        "Moeda Forte
* ---> S4 Migration - 08/07/2023 - MA
*        SELECT *
*          FROM ckmlcr
*          INTO TABLE it_ckmlcr
*           FOR ALL ENTRIES IN it_ckmlhd
*         WHERE curtp EQ '40'
*           AND kalnr EQ it_ckmlhd-kalnr
*           AND bdatj IN s_gjahr
*           AND poper IN s_perio.
        REFRESH lt_ckmlcr[].

        LOOP AT it_ckmlhd INTO wa_ckmlhd.
          wa_kalnr-kalnr = wa_ckmlhd-kalnr.
          APPEND wa_kalnr TO lt_kalnr.
        ENDLOOP.

        CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
*        EXPORTING
*          i_bdatj_1               = lv_bdatj_1
*          i_poper_1               = lv_poper_1
          TABLES
            t_kalnr                 = lt_kalnr
*           t_ckmlpp                = lt_ckmlpp
            t_ckmlcr                = lt_ckmlcr
          EXCEPTIONS
            no_data_found           = 1
            input_data_inconsistent = 2
            buffer_inconsistent     = 3
            OTHERS                  = 4.

        IF lt_ckmlcr[] IS NOT INITIAL.

          DELETE lt_ckmlcr WHERE  bdatj NOT IN s_gjahr  AND poper NOT IN s_perio AND curtp NE '40'.

          IF sy-subrc = 0 AND lines( lt_ckmlcr[] ) > 0.

            MOVE-CORRESPONDING lt_ckmlcr[] TO it_ckmlcr[].
            sy-dbcnt = lines( lt_ckmlcr[] ).
          ELSE.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ENDIF.
        ENDIF.
* <--- S4 Migration - 08/07/2023 - MA

    ENDCASE.
* ---> S4 Migration - 08/07/2023 - MA
*    SELECT *
*      FROM ckmlpp
*      INTO TABLE it_ckmlpp
*       FOR ALL ENTRIES IN it_ckmlhd
*     WHERE kalnr EQ it_ckmlhd-kalnr
*       AND bdatj IN s_gjahr
*       AND poper IN s_perio.

    DATA: lt_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.

    LOOP AT it_ckmlhd INTO wa_ckmlhd.
      wa_kalnr-kalnr = wa_ckmlhd-kalnr.
      APPEND wa_kalnr TO lt_kalnr.
    ENDLOOP.

    CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
*        EXPORTING
*          i_bdatj_1               = lv_bdatj_1
*          i_poper_1               = lv_poper_1
      TABLES
        t_kalnr                 = lt_kalnr
        t_ckmlpp                = lt_ckmlpp
*       t_ckmlcr                = lt_ckmlcr
      EXCEPTIONS
        no_data_found           = 1
        input_data_inconsistent = 2
        buffer_inconsistent     = 3
        OTHERS                  = 4.

    IF lt_ckmlpp[] IS NOT INITIAL.

      DELETE lt_ckmlpp WHERE  bdatj NOT IN s_gjahr  AND poper NOT IN s_perio.

      IF sy-subrc = 0 AND lines( lt_ckmlpp[] ) > 0.

        MOVE-CORRESPONDING lt_ckmlpp[] TO it_ckmlpp[].
        sy-dbcnt = lines( lt_ckmlpp[] ).
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.
    ENDIF.
* <--- S4 Migration - 08/07/2023 - MA

    SELECT *
      FROM mbew
      INTO TABLE it_mbew
     WHERE matnr IN s_matnr
       AND bwkey IN s_werks.

    SELECT *
      FROM t030
      INTO TABLE it_t030
       FOR ALL ENTRIES IN it_mbew
     WHERE ktopl EQ '0050'
       AND bklas EQ it_mbew-bklas.

    SELECT *
      FROM bsis
      INTO CORRESPONDING FIELDS OF TABLE it_bsis
       FOR ALL ENTRIES IN it_t030
      WHERE bukrs IN s_bukrs
        AND hkont EQ it_t030-konts
        AND gjahr IN s_gjahr
        AND monat IN s_perio.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf
       FOR ALL ENTRIES IN it_bsis
      WHERE bukrs EQ it_bsis-bukrs
        AND belnr EQ it_bsis-belnr
        AND gjahr EQ it_bsis-gjahr.

    LOOP AT it_bkpf INTO wa_bkpf.
      READ TABLE it_bsis INTO wa_bsis WITH KEY bukrs = wa_bkpf-bukrs
                                               belnr = wa_bkpf-belnr
                                               gjahr = wa_bkpf-gjahr.
      IF sy-subrc IS INITIAL.
        wa_bsis-mblnr = wa_bkpf-awkey(10).
        MOVE wa_bsis-buzei TO wa_bsis-zeile.
        MODIFY it_bsis FROM wa_bsis INDEX sy-tabix TRANSPORTING mblnr zeile.
      ENDIF.
    ENDLOOP.

    SELECT *
      FROM mseg
      INTO TABLE it_mseg
       FOR ALL ENTRIES IN it_bsis
     WHERE mblnr EQ it_bsis-mblnr
       AND mjahr EQ it_bsis-gjahr.
*     AND ZEILE EQ IT_BSIS-BUZEI.

    SELECT *
      INTO TABLE it_zmmt0060
      FROM zmmt0060.
    "  FOR ALL ENTRIES IN IT_MSEG
    "WHERE BWART EQ IT_MSEG-BWART.

    SELECT *
      FROM t001w
      INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_mseg
      WHERE werks EQ it_mseg-werks.



    SELECT *
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_mseg
      WHERE matnr EQ it_mseg-matnr
        AND spras EQ sy-langu.

  ENDIF.
ENDFORM.                    "F_SELECIONA_DADOS
"F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text                                                 *
*  <--  p2        text                                                 *
*----------------------------------------------------------------------*
FORM f_organizar_dados.

  DATA: nome_campo TYPE lvc_fname,
        desc_campo TYPE scrtext_l.

  DATA: lv_num TYPE i VALUE 0,
        valor  TYPE p_waers,
        e      TYPE zmmt0060-tipo,
        s      TYPE zmmt0060-tipo.

*  READ TABLE IT_CKMLHD INTO WA_CKMLHD WITH KEY  KALNR = WA_CKMLHD-KALNR.

*  READ TABLE IT_CKMLPP INTO WA_CKMLPP WITH KEY LBKUM = WA_CKMLPP-LBKUM.


*  READ TABLE IT_MBEW INTO WA_MBEW WITH KEY BKLAS = WA_MBEW-BKLAS.

*  READ TABLE IT_T030 INTO WA_T030 WITH KEY KONTS = WA_T030-KONTS.


  LOOP AT it_bsis INTO wa_bsis.
    READ TABLE it_mseg INTO wa_mseg WITH KEY mblnr = wa_bsis-mblnr
                                             mjahr = wa_bsis-gjahr.
*                                             ZEILE = WA_BSIS-ZEILE.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE it_zmmt0060 INTO wa_zmmt0060 WITH KEY bwart = wa_mseg-bwart.
    IF sy-subrc IS INITIAL.
      CLEAR: wa_totais.

      wa_totais-werks      = wa_mseg-werks.       "Bucar da MSEG - Centro Virtual/Real

      "le a tabela t001w faz a busca do centro real com base no werks (centro)
      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_totais-werks.
      wa_totais-j_1bbranch = wa_t001w-j_1bbranch. "Buscar da T001W - Centro Real

      "le a tabela total com base nos campos centro,centro real,descrição e no tipo.
      READ TABLE it_totais INTO wa_totais  WITH KEY          werks = wa_totais-werks
                                                        j_1bbranch = wa_totais-j_1bbranch
                                                    descr_abertura = wa_zmmt0060-descr_abertura
                                                              tipo = wa_zmmt0060-tipo.
      "totaliza o campo valor e quantidade de acordo com a moeda escolhida
      " depois de totalizado mostra ao usuario o total em quantidade separado pelo tipo e pela descriçao
      "entrada ou saida
      IF sy-subrc IS INITIAL.
        CASE p_waers.
          WHEN 'BRL'.
            wa_totais-valor = wa_totais-valor + wa_bsis-dmbtr.     "BSIS-DMBTR  / BSIS-DMBE2
          WHEN 'USD'.
            wa_totais-valor = wa_totais-valor + wa_bsis-dmbe2.     "BSIS-DMBTR  / BSIS-DMBE2
        ENDCASE.
        wa_totais-quant = wa_totais-quant + wa_mseg-menge.     "MSEG-MENGE
        MODIFY it_totais FROM wa_totais INDEX sy-tabix TRANSPORTING valor quant.
      ELSE.
        ADD 1 TO lv_num.
        wa_totais-numero         = lv_num.
        CASE p_waers.
          WHEN 'BRL'.
            wa_totais-valor          = wa_bsis-dmbtr. "BSIS-DMBTR  / BSIS-DMBE2
          WHEN 'USD'.
            wa_totais-valor          = wa_bsis-dmbe2. "BSIS-DMBTR  / BSIS-DMBE2
        ENDCASE.
        wa_totais-quant          = wa_mseg-menge. "MSEG-MENGE
        wa_totais-descr_abertura = wa_zmmt0060-descr_abertura.
        wa_totais-tipo           = wa_zmmt0060-tipo.
        APPEND wa_totais TO it_totais.
      ENDIF.

    ENDIF.

  ENDLOOP.

  "mostra os dados na Alv
  PERFORM f_alv.
  ""
  lv_num = 0.

  "deleta duplicidade em um devido campo da tabela
*---> 04/07/2023 - Migração S4 - WS
  SORT it_zmmt0060 BY descr_abertura tipo.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM it_zmmt0060 COMPARING descr_abertura tipo.

  "le a tabela zmmt0060 busca os dados com o campo especifico do tipo entrada.
  LOOP AT it_zmmt0060 INTO wa_zmmt0060 WHERE tipo EQ 'E'.
    READ TABLE it_totais INTO wa_totais
     WITH KEY descr_abertura = wa_zmmt0060-descr_abertura
              tipo           = wa_zmmt0060-tipo.
    "busca o valor e quantidade e armazena nos campos quant e vlr
    "se ele achar o valor atribui os resultados ao campo senao faz uma nova pesquisa
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    ADD 1 TO lv_num.
    nome_campo = lv_num.
    SHIFT nome_campo LEFT DELETING LEADING ' '.
    CONCATENATE 'QUANT'  nome_campo INTO nome_campo.
    CONCATENATE wa_zmmt0060-descr_abertura '(Kg)' INTO desc_campo.
    PERFORM alv_preenche_cat USING: nome_campo desc_campo '12'  '' ''   ''.

    nome_campo = lv_num.
    SHIFT nome_campo LEFT DELETING LEADING ' '.
    CONCATENATE 'VALOR'  nome_campo INTO nome_campo.
    CONCATENATE wa_zmmt0060-descr_abertura '(Vlr)' INTO desc_campo.
    PERFORM alv_preenche_cat USING: nome_campo desc_campo  '12'  '' ''   ''.

    LOOP AT it_t001w INTO wa_t001w.
      LOOP AT it_totais INTO wa_totais  WHERE werks          EQ wa_t001w-werks
                                          AND j_1bbranch     EQ wa_t001w-j_1bbranch
                                          AND descr_abertura EQ wa_zmmt0060-descr_abertura
                                          AND tipo           EQ wa_zmmt0060-tipo.

        READ TABLE it_saida INTO wa_saida WITH KEY      werks = wa_t001w-werks
                                                   j_1bbranch = wa_t001w-j_1bbranch.
        IF sy-subrc IS INITIAL.
          PERFORM inclui_valor USING lv_num wa_totais CHANGING wa_saida.
          "faz a busca se achar passa para o comando field symbol,para execução da funçao e retorna com os valores
          "cada um em seu campo de saida com filtro em centro nome descrição de abertura.
          MODIFY it_saida INDEX sy-tabix FROM wa_saida.
        ELSE.
          CLEAR: wa_saida.
          wa_saida-werks = wa_t001w-werks.
          wa_saida-name1 = wa_t001w-name1.
          wa_saida-j_1bbranch = wa_t001w-j_1bbranch.
          wa_saida-descr_abertura = wa_zmmt0060-descr_abertura.
          PERFORM inclui_valor USING lv_num wa_totais CHANGING wa_saida.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  "le a tabela zmmt0060 busca os dados com o campo especifico do tipo saida.
  LOOP AT it_zmmt0060 INTO wa_zmmt0060 WHERE tipo EQ 'S'.

    READ TABLE it_totais INTO wa_totais
     WITH KEY descr_abertura = wa_zmmt0060-descr_abertura
              tipo           = wa_zmmt0060-tipo.

    "busca o valor e quantidade e armazena nos campos quant e vlr
    "se ele achar o valor atribui os resultados ao campo senao faz uma nova pesquisa
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    ADD 1 TO lv_num.

    MOVE lv_num TO nome_campo.
    SHIFT nome_campo LEFT DELETING LEADING ' '.
    CONCATENATE 'QUANT'  nome_campo INTO nome_campo.
    CONCATENATE wa_zmmt0060-descr_abertura '(Kg)' INTO desc_campo.
    PERFORM alv_preenche_cat USING: nome_campo desc_campo '12'  '' ''   ''.

    MOVE lv_num TO nome_campo.
    SHIFT nome_campo LEFT DELETING LEADING ' '.
    CONCATENATE 'VALOR'  nome_campo INTO nome_campo.
    CONCATENATE wa_zmmt0060-descr_abertura '(Vlr)' INTO desc_campo.
    PERFORM alv_preenche_cat USING: nome_campo desc_campo  '12'  '' ''   ''.


    LOOP AT it_t001w INTO wa_t001w.
      LOOP AT it_totais INTO wa_totais  WHERE werks          EQ wa_t001w-werks
                                          AND j_1bbranch     EQ wa_t001w-j_1bbranch
                                          AND descr_abertura EQ wa_zmmt0060-descr_abertura
                                          AND tipo           EQ wa_zmmt0060-tipo.

        READ TABLE it_saida INTO wa_saida WITH KEY      werks = wa_t001w-werks
                                                   j_1bbranch = wa_t001w-j_1bbranch.
        IF sy-subrc IS INITIAL.
          PERFORM inclui_valor USING lv_num wa_totais CHANGING wa_saida.
          "faz a busca se achar passa para o comando field symbol,para execução da funçao e retorna com os valores
          "cada um em seu campo de saida com filtro em centro nome descrição de abertura.
          MODIFY it_saida INDEX sy-tabix FROM wa_saida.
        ELSE.
          CLEAR: wa_saida.
          wa_saida-werks = wa_t001w-werks.
          wa_saida-name1 = wa_t001w-name1.
          wa_saida-j_1bbranch = wa_t001w-j_1bbranch.
          wa_saida-descr_abertura = wa_zmmt0060-descr_abertura.
          PERFORM inclui_valor USING lv_num wa_totais CHANGING wa_saida.
          APPEND wa_saida TO it_saida.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.


*        LOOP AT IT_TOTAIS.

*        ENDLOOP.
  APPEND wa_saida TO it_saida.

ENDFORM.                    "F_ORGANIZAR_DADOS

*=============================================================================*
*Form F_Alv                                                                   *
*=============================================================================*
FORM f_alv.

*  DATA: NOME_CAMPO TYPE LVC_FNAME,
*        LV_NUM     TYPE I,
*        DESC_CAMPO TYPE SCRTEXT_L.


  PERFORM alv_preenche_cat USING:
        'J_1BBRANCH'   'Centro FX'                 '12'  '' ''   '',
        'NAME1'        'Nome Centro Fixo'          '12'  '' ''   '',
        'WERKS'        'Centro'                    '12'  '' ''   ''.


ENDFORM.                    "F_ALV

"&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT                                         *
*&---------------------------------------------------------------------*
FORM alv_preenche_cat   USING   p_campo TYPE c
                                p_desc  TYPE c
                                p_tam   TYPE c
                                p_hot   TYPE c
                                p_zero  TYPE c
                                p_sum   TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum   =  p_sum.
  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = space.
  gs_variant-log_group   = space.
  gs_variant-username    = space.
  gs_variant-variant     = space.
  gs_variant-text        = space.
  gs_variant-dependvars  = space.

ENDFORM.                    " FILL_GS_VARIANT

"&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT                                    *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.
  MESSAGE s000.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT                                        *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .

        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.

    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  INCLUI_VALOR                                             *
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*      -->P_LV_NUM  text                                               *
*      -->P_WA_TOTAIS  text                                            *
*      <--P_WA_SAIDA  text                                             *
*----------------------------------------------------------------------*
FORM inclui_valor  USING    p_lv_num    TYPE i
                            p_wa_totais TYPE ty_totais
                   CHANGING p_wa_saida  TYPE ty_saida.

  DATA: lc TYPE c LENGTH 512,
        lq TYPE c LENGTH 512.

  FIELD-SYMBOLS: <valor> TYPE dmbtr,
                 <quant> TYPE mseg-menge.


  MOVE p_lv_num TO lc.
  SHIFT lc LEFT DELETING LEADING ' '.
  CONCATENATE 'WA_SAIDA-VALOR' lc INTO lc.
  ASSIGN (lc) TO <valor>.

  MOVE p_lv_num TO lq.
  SHIFT lq LEFT DELETING LEADING ' '.
  CONCATENATE 'WA_SAIDA-QUANT' lq INTO lq.
  ASSIGN (lq) TO <quant>.

  CASE p_lv_num.
    WHEN 1.
      <valor> = <valor> + wa_totais-valor.
      <quant> = <quant> + wa_totais-quant.
  ENDCASE.
ENDFORM.                    " INCLUI_VALOR

*&---------------------------------------------------------------------*
*&      Form  CONTAINER_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM container_html .

  DATA : dl_length        TYPE i,                           " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html1.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document.

  CALL METHOD dg_dyndoc_id->set_document_background
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.

  CALL METHOD dg_dyndoc_id->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html1
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.                    " CONTAINER_HTML



*&---------------------------------------------------------------------*
*&      Form  ADD_TEXT
*&---------------------------------------------------------------------*
*       To add Text
*----------------------------------------------------------------------*
FORM add_text USING p_text  TYPE sdydo_text_element
                    p_style TYPE sdydo_attribute
                    p_size  TYPE sdydo_attribute
                    p_color TYPE sdydo_attribute.

* Adding text
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text          = p_text
      sap_style     = p_style
      sap_fontsize  = p_size
      sap_color     = p_color
      sap_fontstyle = cl_dd_area=>sans_serif.

  "SAP_STYLE    = CL_DD_AREA=>HEADING
  "SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
  "SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
ENDFORM.                    " ADD_TEXT


*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_objects OUTPUT.

  DATA: url(255) TYPE c.

* Create container and ALV objects only once
  IF gf_first_display = 'X'.

*   Create object for container
    CREATE OBJECT ctl_cccontainer
      EXPORTING
        container_name = 'TELA_0100'.

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = ctl_cccontainer
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_html
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html1.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_html2.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_html2.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_grid.

    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 15.


*   Create object for ALV grid inside container
    CREATE OBJECT ctl_alv_resumo
      EXPORTING
        i_parent = dg_parent_grid.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

    "GS_LAYOUT-SEL_MODE = 'A'.
    gs_layout-zebra      = 'X'.
    "GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.

*   Create Object for Event Handler
    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv_resumo.
*    SET HANDLER EVENT_HANDLER->TOP_OF_PAGE          FOR CTL_ALV_RESUMO.
    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = dg_parent_grid
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

**   Send data to ALV grid
    CALL METHOD ctl_alv_resumo->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        is_variant           = gs_variant
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida.

    PERFORM cria_html_cab.

    CALL METHOD ctl_alv_resumo->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = dg_dyndoc_id.

    CLEAR: gf_first_display.

  ENDIF.

  CALL METHOD ctl_alv_resumo->refresh_table_display.

  CALL METHOD ctl_alv_resumo->set_scroll_info_via_id
    EXPORTING
      is_col_info = gs_scroll_col
      is_row_no   = gs_scroll_row.

ENDMODULE.                 " CREATE_OBJECTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_HTML_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_html_cab .

  DATA: column                  TYPE REF TO cl_dd_area,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        table_element           TYPE REF TO cl_dd_table_element,
        table_element2          TYPE REF TO cl_dd_table_element,
        p_text                  TYPE sdydo_text_element,
        p_text_table            TYPE sdydo_text_table,
        sdydo_text_element(255),
        vg_mes(2), vg_ano(4),
        qtd                     TYPE i.

  CALL METHOD dg_dyndoc_id->initialize_document.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 1
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element.

  CALL METHOD table_element->add_column
    IMPORTING
      column = column.

  CALL METHOD table_element->set_column_style
    EXPORTING
      col_no    = 1
      sap_align = 'CENTER'
      sap_style = cl_dd_document=>heading.

  p_text = 'Relatório Kardex'.
  CALL METHOD column->add_text
    EXPORTING
      text      = p_text
      sap_style = 'HEADING'.

  CALL METHOD dg_dyndoc_id->add_table
    EXPORTING
      no_of_columns = 2
      border        = '0'
      width         = '100%'
    IMPORTING
      table         = table_element2.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_1.

  CALL METHOD table_element2->add_column
    IMPORTING
      column = column_2.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 1
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  CALL METHOD table_element2->set_column_style
    EXPORTING
      col_no       = 2
      sap_align    = 'LEFT'
      sap_fontsize = cl_dd_document=>medium.

  sdydo_text_element = 'Empresa:'.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Periodo(s): '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Material: '.
  APPEND sdydo_text_element TO p_text_table.

  sdydo_text_element = 'Moeda: '.
  APPEND sdydo_text_element TO p_text_table.


  sdydo_text_element = 'Data: '.
  APPEND sdydo_text_element TO p_text_table.

  CALL METHOD column_1->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.

  CLEAR: p_text_table, sdydo_text_element.

  "Empresa*********
  DESCRIBE TABLE s_bukrs LINES qtd.

  IF qtd = 1.
    READ TABLE s_bukrs INDEX 1.
    SELECT SINGLE * INTO wa_bsis FROM bsis WHERE bukrs EQ s_bukrs-low.
    sdydo_text_element = wa_bsis-bukrs.
  ELSE.
    LOOP AT s_bukrs.
      IF sdydo_text_element IS INITIAL.
        SELECT SINGLE * INTO wa_bsis FROM bsis WHERE bukrs EQ s_bukrs-low.
        sdydo_text_element = s_bukrs-low.
      ELSE.
        CONCATENATE sdydo_text_element 'Empresa' INTO sdydo_text_element SEPARATED BY space.
        CONCATENATE sdydo_text_element s_bukrs-low INTO sdydo_text_element.
      ENDIF.
    ENDLOOP.
  ENDIF.
  APPEND sdydo_text_element TO p_text_table.

  "periodo*****
  IF s_perio IS NOT INITIAL.
    CONCATENATE s_perio-low(2) '/'s_gjahr-low(4) INTO sdydo_text_element.
  ENDIF.
  IF s_perio-high IS NOT INITIAL.
    CONCATENATE sdydo_text_element '-' s_perio-high(2) INTO sdydo_text_element SEPARATED BY space.
    CONCATENATE sdydo_text_element '/'s_gjahr-low(4) INTO sdydo_text_element.
  ENDIF.
  APPEND sdydo_text_element TO p_text_table.

  " Material*********
  sdydo_text_element = s_matnr-low.
  APPEND sdydo_text_element TO p_text_table.

  "Moeda*********
  sdydo_text_element = p_waers.
  APPEND sdydo_text_element TO p_text_table.

  "DATA*********
  WRITE sy-datum TO sdydo_text_element.
  APPEND sdydo_text_element TO p_text_table.

  CALL METHOD column_2->add_text
    EXPORTING
      text_table = p_text_table
      fix_lines  = 'X'.





  PERFORM container_html.

ENDFORM.                    " CRIA_HTML_CAB

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM
