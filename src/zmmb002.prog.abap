
************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 17.04.2007                                          *
* Tipo de prg ...: Carga de Dados com bapi                             *
* Objetivo    ...: Carga de saldo de materiais                         *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 17.04.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
REPORT zmmb002 LINE-SIZE 150
               LINE-COUNT 62(03)
               MESSAGE-ID z01 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
*
INCLUDE <icon>.

DATA: v_message(220) TYPE c,
      v_mess(256)    TYPE c,
      v_bapiret2     LIKE bapiret2,
      v_ctrlcol1     TYPE alsmex_tabline-value,
      v_cont         TYPE i,
      v_old_bwart    LIKE rm07m-bwartwa,
      v_old_matnr    LIKE mara-matnr,
      v_icon         TYPE c.

FIELD-SYMBOLS <icone>    LIKE icon_checked.

*----------------------------------------------------------------------*
* Declaração para Batch_input de determinação de CFOP
*----------------------------------------------------------------------*
*
DATA: wa_bdcdata LIKE bdcdata,
      wa_messtab LIKE bdcmsgcoll,
      t_bdcdata  LIKE STANDARD TABLE OF wa_bdcdata,
      t_messtab  LIKE STANDARD TABLE OF wa_messtab.

*----------------------------------------------------------------------*
* Declaração para função GET_MATERIAL_ID
*----------------------------------------------------------------------*
DATA: i_bismt LIKE mara-bismt,
      t_matid LIKE mara        OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*

DATA: wa_planilha LIKE alsmex_tabline,
      t_planilha  LIKE STANDARD TABLE OF wa_planilha.

DATA: BEGIN OF wa_datxls,
        line   LIKE sy-tabix,             "linha da planilha
        bldat  LIKE mkpf-bldat,           "Data no documento
        budat  LIKE mkpf-budat,           "Data de lançamento
        bktxt  LIKE mkpf-bktxt,           "Texto Cab. Documto
        bwart  LIKE rm07m-bwartwa,        "Tipo de movimento
        werks  LIKE rm07m-werks,          "Centro
        matnr  LIKE mseg-matnr,           "Material
        mtart  LIKE mara-mtart,           "Tipo de material
        erfmg  LIKE mseg-erfmg,           "Quantidade
        erfme  LIKE mseg-erfme,           "Unidade de medida
        lgort  LIKE rm07m-lgort,          "Depósito
        charg  LIKE mseg-charg,           "Lote
        bwtar  LIKE bwtar-bwtar,          "Tipo de avaliação
        hsdat  LIKE dm07m-hsdat_input,    "Dt produção do lote
        vfdat  LIKE dm07m-vfdat_input,    "Data do vencimento
        grund  LIKE rm07m-grund,          "Motivo do movimento
        amount LIKE
                 bapi2017_gm_item_create-amount_lc, "Preço
      END   OF wa_datxls,

      BEGIN OF wa_arqtxt,
        bldat(10),
        budat(10),
        bktxt(25),
        bwart(03),
        werks(04),
*---> 02/06/2023 - Migração S4 - JS
*         matnr(18),  "Old id matnr (sigam)
        matnr(40),
*<--- 02/06/2023 - Migração S4 - JS
        mtart(04),
        erfmg(18),
        erfme(03),
        lgort(04),
        charg(10),
        bwtar(10),
        hsdat(10),
        vfdat(10),
        grund(04),
        amount(30),
      END   OF wa_arqtxt,

      t_datxls LIKE STANDARD TABLE OF wa_datxls,
      t_arqtxt LIKE STANDARD TABLE OF wa_arqtxt.

DATA: v_ncoln            LIKE sy-index VALUE 30.

CONSTANTS: c_ncoln     LIKE sy-index VALUE 01,
           c_nline     LIKE sy-index VALUE 01,
           c_watxt(10) TYPE c        VALUE 'WA_ARQTXT-',
           c_waarq(10) TYPE c        VALUE 'WA_DATXLS-'.

FIELD-SYMBOLS: <fs_datain>  TYPE any,
               <fs_dataout> TYPE any.

*----------------------------------------------------------------------*
* Declaração para função BAPI_GOODSMVT_CREATE
*----------------------------------------------------------------------*

DATA: t_header  LIKE bapi2017_gm_head_01,
      t_code    LIKE bapi2017_gm_code,
      t_headret LIKE bapi2017_gm_head_ret,

      wa_item   LIKE bapi2017_gm_item_create,
      wa_return LIKE bapiret2,
      t_item    LIKE STANDARD TABLE OF bapi2017_gm_item_create,
      t_return  LIKE STANDARD TABLE OF bapiret2.



*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_cami  LIKE rlgrap-filename.    "Arquivo Excel
  PARAMETERS: p_nline LIKE sy-index.           "Nro aproximado de Linhas
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_optxt RADIOBUTTON GROUP arq DEFAULT 'X',
              p_opxls RADIOBUTTON GROUP arq.
SELECTION-SCREEN END   OF BLOCK b2.

*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
INITIALIZATION.
  SET TITLEBAR 'TITLE01'.
  IF p_nline IS INITIAL.
    p_nline = 9999.
  ENDIF.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cami.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_cami
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    IMPORTING
      filename         = p_cami
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
START-OF-SELECTION.
  PERFORM f_processa_planilha.
  SORT t_datxls BY line.
  CLEAR: v_old_bwart, v_old_matnr.

  LOOP AT t_datxls INTO wa_datxls.
    IF ( NOT wa_datxls-bwart IS INITIAL ).
      IF ( NOT  v_old_bwart IS INITIAL ).
        PERFORM f_move_goods.
      ENDIF.
      v_old_bwart = wa_datxls-bwart.
      v_old_matnr = wa_datxls-matnr.
    ENDIF.

    PERFORM f_get_date_for_bapi.
  ENDLOOP.

  PERFORM f_move_goods.


*----------------------------------------------------------------------*
* Event end-of-selection
*----------------------------------------------------------------------*
*
END-OF-SELECTION.
  ULINE.
*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
TOP-OF-PAGE.

  ULINE.
  WRITE: /01 sy-vline,
          30 'Log erros - MM/MB1C Carga de Saldos de Material',
          99 'Data.:',
         106 sy-datum,
         119 'Hora.:',
         126 sy-uzeit,
         137 'Pag.:',
         143 sy-pagno,
         150 sy-vline.
  ULINE.
  WRITE: /01 sy-vline,
          03 'Nome do arquivo importado:',
         150 sy-vline.
  WRITE: /01 sy-vline,
          03 'Nro de linhas do arquivo :',
          99 'Usuário:',
         108 sy-uname,
         119 'Programa:',
         129 sy-repid,
         150 sy-vline.
  ULINE.
  WRITE: /01 sy-vline,
          05 'linha',
          11 'Material',
          25 'Mensagem do log',
         150 sy-vline.
  ULINE.

*----------------------------------------------------------------------*
* Event END_OF_PAGE.
*----------------------------------------------------------------------*
*
END-OF-PAGE.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilha
*&---------------------------------------------------------------------*
FORM f_processa_planilha.

*
* Carrega planilha com dados de saldo de material
*
  DATA: vl_subrc      LIKE sy-subrc,
        vl_cami       TYPE string,
        vl_line       TYPE i,
        vl_campo(40),
        vl_campo2(40).


  CLEAR   t_planilha.
  REFRESH t_planilha.


  IF ( p_optxt EQ 'X' ).
*
* Carrega arquivo texto com dados de Saldo de estoque Material
*
    REFRESH t_arqtxt.
    vl_cami = p_cami.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = vl_cami
      TABLES
        data_tab                = t_arqtxt
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF ( sy-subrc NE 0 ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    CLEAR vl_line.

    LOOP AT t_arqtxt INTO wa_arqtxt.
      CLEAR wa_datxls.
      vl_line = vl_line + 1.

      CHECK ( vl_line LE p_nline ).

      PERFORM f_get_material USING wa_arqtxt-matnr
                          CHANGING wa_arqtxt-matnr
                                   wa_arqtxt-mtart.
      IF ( wa_arqtxt-matnr IS INITIAL ).
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input        = wa_arqtxt-matnr
        IMPORTING
          output       = wa_arqtxt-matnr
        EXCEPTIONS
          length_error = 1
          OTHERS       = 2.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = wa_arqtxt-erfme
        IMPORTING
          output         = wa_arqtxt-erfme
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.

      REPLACE '.' WITH ' ' INTO wa_arqtxt-erfmg.
      REPLACE ',' WITH '.' INTO wa_arqtxt-erfmg.
      CONDENSE wa_arqtxt-erfmg NO-GAPS.

      wa_datxls-line     = vl_line.

      REPLACE '.' WITH ' ' INTO wa_arqtxt-amount.
      REPLACE ',' WITH '.' INTO wa_arqtxt-amount.
      CONDENSE wa_arqtxt-amount NO-GAPS.

      MOVE-CORRESPONDING wa_arqtxt TO wa_datxls.


      DO 4 TIMES.
        CASE sy-index.
          WHEN 1.
            CONCATENATE c_watxt 'BLDAT' INTO vl_campo.
            CONCATENATE c_waarq 'BLDAT' INTO vl_campo2.
          WHEN 2.
            CONCATENATE c_watxt 'BUDAT' INTO vl_campo.
            CONCATENATE c_waarq 'BUDAT' INTO vl_campo2.
          WHEN 3.
            CONCATENATE c_watxt 'HSDAT' INTO vl_campo.
            CONCATENATE c_waarq 'HSDAT' INTO vl_campo2.
          WHEN 4.
            CONCATENATE c_watxt 'VFDAT' INTO vl_campo.
            CONCATENATE c_waarq 'VFDAT' INTO vl_campo2.
        ENDCASE.

        ASSIGN (vl_campo)  TO <fs_datain>.
        ASSIGN (vl_campo2) TO <fs_dataout>.

        IF ( <fs_datain>     IS ASSIGNED ) AND
           ( NOT <fs_datain> IS INITIAL  ).
          TRANSLATE <fs_datain> USING '/.'.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = <fs_datain>
            IMPORTING
              date_internal = <fs_dataout>.
        ENDIF.
      ENDDO.

      APPEND wa_datxls TO t_datxls.
    ENDLOOP.

  ELSE.

*> Carrega planilha selecionada para tabela interna
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_cami
        i_begin_col             = c_ncoln
        i_begin_row             = c_nline
        i_end_col               = v_ncoln
        i_end_row               = p_nline
      TABLES
        intern                  = t_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE e004 WITH TEXT-001.
    ENDIF.
*> Elimina linhas de cabeçalho e texto da tabela interna.
*  delete t_planilha where ( col   eq 1 )
*                      and ( value eq 'CAB' ).
*
    SORT t_planilha BY row col.
    REFRESH t_datxls.
    CLEAR   t_datxls.

*> Move valores das células ti com dados de criação dos materiais
    LOOP AT t_planilha INTO wa_planilha.

      ON CHANGE OF wa_planilha-row.
        v_ctrlcol1 = wa_planilha-value.
      ENDON.

      IF v_ctrlcol1 EQ 'CAB'.
        DELETE t_planilha.
        CONTINUE.
      ENDIF.

      AT NEW row.
        CLEAR wa_datxls.
        wa_datxls-line = wa_planilha-row.
      ENDAT.


      CASE wa_planilha-col.
        WHEN   2. wa_datxls-bldat = wa_planilha-value.
        WHEN   3. wa_datxls-budat = wa_planilha-value.
        WHEN   4. wa_datxls-bktxt = wa_planilha-value.
        WHEN   5. wa_datxls-bwart = wa_planilha-value.
        WHEN   6. wa_datxls-werks = wa_planilha-value.
        WHEN   7.
          PERFORM f_get_material USING wa_planilha-value
                                       CHANGING wa_datxls-matnr
                                               wa_datxls-mtart.
        WHEN   8.
          REPLACE '.' WITH ' ' INTO wa_planilha-value.
          REPLACE ',' WITH '.' INTO wa_planilha-value.
          CONDENSE wa_planilha-value NO-GAPS.

          wa_datxls-erfmg = wa_planilha-value.
        WHEN   9.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input    = wa_planilha-value
              language = sy-langu
            IMPORTING
              output   = wa_datxls-erfme.

        WHEN  10. wa_datxls-lgort = wa_planilha-value.
        WHEN  11. wa_datxls-charg = wa_planilha-value.
        WHEN  12.
          TRANSLATE wa_planilha-value TO UPPER CASE.
          wa_datxls-bwtar = wa_planilha-value.
        WHEN  13.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = wa_planilha-value
            IMPORTING
              date_internal = wa_datxls-hsdat.
        WHEN  14.
          CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
            EXPORTING
              date_external = wa_planilha-value
            IMPORTING
              date_internal = wa_datxls-vfdat.
        WHEN  19. wa_datxls-grund = wa_planilha-value.
        WHEN  20.
          REPLACE '.' WITH ' ' INTO wa_planilha-value.
          REPLACE ',' WITH '.' INTO wa_planilha-value.
          CONDENSE wa_planilha-value NO-GAPS.

          wa_datxls-amount = wa_planilha-value.

      ENDCASE.

      AT END OF row.
        APPEND wa_datxls TO t_datxls.
      ENDAT.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " f_processa_planilha

*&---------------------------------------------------------------------*
*&      Form  f_get_date_for_bapi
*&---------------------------------------------------------------------*
FORM f_get_date_for_bapi.

  IF ( NOT wa_datxls-bwart IS INITIAL ).
    CLEAR    : t_header, t_code  , t_headret, wa_item.
    REFRESH  : t_item,   t_return.
    IF wa_datxls-budat IS INITIAL.
      t_header-pstng_date    = sy-datum.
    ELSE.
      t_header-pstng_date    = wa_datxls-budat.
    ENDIF.
    IF wa_datxls-bldat IS INITIAL.
      t_header-doc_date      = sy-datum.
    ELSE.
      t_header-doc_date      = wa_datxls-bldat.
    ENDIF.
    t_header-pr_uname        = sy-uname.
    t_header-header_txt      = wa_datxls-bktxt.
    t_header-ver_gr_gi_slipx = 'X'.

    t_code-gm_code           = '05'.

  ENDIF.


*---> 06/07/2023 - Migração S4 - MA
*          WA_ITEM-MATERIAL         = WA_DATXLS-MATNR.

  DATA(v_len) = strlen( wa_datxls-matnr ).

  IF v_len > 18.
    wa_item-material_long = wa_datxls-matnr.
  ELSE.
    wa_item-material       = wa_datxls-matnr.
  ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

  wa_item-plant            = wa_datxls-werks.
  wa_item-stge_loc         = wa_datxls-lgort.
  wa_item-batch            = wa_datxls-charg.
  wa_item-move_type        = v_old_bwart.
  IF ( wa_datxls-mtart EQ 'ZMPN' ).
    wa_datxls-bwtar         = 'MERCINTERN'.
  ELSEIF ( wa_datxls-mtart EQ 'ZMPI' ).
    wa_datxls-bwtar         = 'MERCEXTERN'.
  ELSE.
    CLEAR wa_datxls-bwtar.
  ENDIF.
  wa_item-val_type         = wa_datxls-bwtar.
  wa_item-entry_qnt        = wa_datxls-erfmg.
  wa_item-entry_uom        = wa_datxls-erfme.
  wa_item-prod_date        = wa_datxls-hsdat.
  wa_item-expirydate       = wa_datxls-vfdat.
  wa_item-move_reas        = wa_datxls-grund.
  wa_item-amount_lc        = wa_datxls-amount.

  APPEND wa_item TO t_item.

ENDFORM.                    " f_get_date_for_bapi
*&---------------------------------------------------------------------*
*&      Form  f_move_goods
*&---------------------------------------------------------------------*
FORM f_move_goods.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = t_header
      goodsmvt_code    = t_code
*     testrun          = 'X'
    IMPORTING
      goodsmvt_headret = t_headret
    TABLES
      goodsmvt_item    = t_item
      return           = t_return.


  READ TABLE t_return INTO wa_return INDEX 1.
  IF ( sy-subrc NE 0 ) AND ( NOT t_headret-mat_doc IS INITIAL ).
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = 'X'
      IMPORTING
        return = wa_return.
    IF NOT wa_return IS INITIAL.
      APPEND wa_return TO t_return.
    ELSE.
      CONCATENATE 'Documento de material ' t_headret-mat_doc
                  'criado com sucesso!' INTO v_message
                                        SEPARATED BY space.
      ASSIGN icon_checked TO <icone>.
      PERFORM f_imprime_erros USING v_message v_old_matnr.
    ENDIF.
  ENDIF.

  LOOP AT t_return INTO wa_return.
    CLEAR v_message.
    IF 'WAE' CS wa_return-type.
      ASSIGN icon_incomplete TO <icone>.
    ELSEIF wa_return-type EQ 'S'.
      ASSIGN icon_checked TO <icone>.
    ELSE.
      CONTINUE.
    ENDIF.


    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = wa_return-id
        number     = wa_return-number
        language   = sy-langu
        textformat = 'ASC'
        message_v1 = wa_return-message_v1
        message_v2 = wa_return-message_v2
        message_v3 = wa_return-message_v3
        message_v4 = wa_return-message_v4
      IMPORTING
        message    = v_message
        return     = v_bapiret2.
    IF sy-subrc NE 0.
      v_message = 'Ocorreu um erro que não pode ser exibido!!'.
    ELSE.
      IF v_message IS INITIAL.
        v_message = v_bapiret2-message.
      ENDIF.
    ENDIF.
    PERFORM f_imprime_erros  USING v_message v_old_matnr.
  ENDLOOP.
ENDFORM.                    " f_move_goods

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
FORM f_imprime_erros USING    p_message p_oldmat.
  WRITE: /01 sy-vline,
          03 <icone> AS ICON,
          05 wa_datxls-line,
          11 p_oldmat,
          25(123) p_message,
         150 sy-vline.

ENDFORM.                    " f_imprime_erros
*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING    VALUE(p_flag)
                          VALUE(p_fnam)
                          VALUE(p_fval).

  CLEAR t_bdcdata.
  IF NOT p_flag IS INITIAL.
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
    wa_bdcdata-dynbegin = 'X'.
  ELSE.
    wa_bdcdata-fnam = p_fnam.
    wa_bdcdata-fval = p_fval.
  ENDIF.
  APPEND wa_bdcdata TO t_bdcdata.

ENDFORM.                    "f_bdc_field
*&---------------------------------------------------------------------*
*&      Form  f_get_material
*&---------------------------------------------------------------------*
FORM f_get_material  USING    VALUE(p_value)
                  CHANGING    p_matnr LIKE mara-matnr
                              p_mtart LIKE mara-mtart.

  DATA: vl_bismt           LIKE mara-bismt.

  vl_bismt = p_value.
  CALL FUNCTION 'GET_MATERIAL_ID'
    EXPORTING
      i_oldmat              = vl_bismt
    TABLES
      x_mara                = t_matid
    EXCEPTIONS
      too_many_input_params = 1
      ean_not_found         = 2
      oldmat_not_found      = 3
      OTHERS                = 4.

  IF sy-subrc EQ 0.
    READ TABLE t_matid INDEX 1.
    p_matnr = t_matid-matnr.
    p_mtart = t_matid-mtart.
  ELSE.
    CLEAR: p_matnr, p_mtart.
    ASSIGN icon_incomplete TO <icone>.
    PERFORM f_imprime_erros  USING TEXT-002 vl_bismt.
    v_ctrlcol1 = 'CAB'.

  ENDIF.

ENDFORM.                    " f_get_material
