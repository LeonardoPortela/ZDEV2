************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 10.04.2007                                          *
* Tipo de prg ...: Carga de Dados com Batch-Input
* Objetivo    ...: Coletar dados de fornecedores de plan.excel e efe-  *
*                  tuar a carga destes no R/3                          *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 10.04.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
REPORT zfib003 LINE-SIZE 150
               LINE-COUNT 62(03)
               MESSAGE-ID z01 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
INCLUDE <icon>.
TYPE-POOLS: sydes.

TABLES: zbccarga_tmp.

CONSTANTS: c_ncoln       LIKE sy-index VALUE 01,
           c_nline       LIKE sy-index VALUE 01,
           c_path        TYPE c LENGTH 200 VALUE '/tmp/'.

DATA: v_mess_tab(256)    TYPE c,
      v_ctrlcol1         TYPE alsmex_tabline-value,
      v_ncoln            LIKE sy-index VALUE 60,
      vg_lifnr           LIKE lfa1-lifnr,
      vg_lifnr_aux       LIKE lfa1-lifnr,
      vg_bukrs           LIKE t001-bukrs,
      vg_filename        LIKE rlgrap-filename,
      vg_totlines        TYPE i,
      vg_index           TYPE n LENGTH 02,
      vg_field_shdb      TYPE c LENGTH 60,
      vg_field_value     TYPE c LENGTH 60.


FIELD-SYMBOLS: <icone>    LIKE icon_checked,
               <fs_value> TYPE ANY.

*----------------------------------------------------------------------*
* Declaração para Batch_input de determinação de CFOP
*----------------------------------------------------------------------*
DATA: t_bdcdata          LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
      t_messtab          TYPE tab_bdcmsgcoll. "LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
DATA: t_planilha         LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF wa_arqtxt,
         lifnr(16),
         bukrs(60),
         ekorg(04),
         ktokk(04),
         title_medi(15),
         name1(40),
         sortl(10),
         street(35),
         house_num1(10),
         city2(40),
         post_code1(10),
         city1(35),
         country(03),
         region(03),
         po_box(10),
         post_code2(10),
         langu(01),
         tel_number(30),
         tel_extens(10),
         mob_number(30),
         fax_number(30),
         fax_extens(10),
         deflt_comm(03),
         remark(50),
         kunnr(10),
         stcd1(16),
         stkzn(01),
         stcd2(11),
         stcd3(18),
         stcd4(18),
         stenr(18),
         banks(03),
         bankl(250),
         bankn(250),
         bkont(060),
         bvtyp(100),
         namev(35),
         name2(35),
         abtnr(03),
         pafkt(02),
         akont(10),
         zuawa(03),
         fdgrv(10),
         altkn(10),
         zterm(04),
         zwels(10),
         zgrup(02),
         qland(03),
         waers(05),
         smtp_addr(241),
         eikto(12),
         zsabe(15),
      END OF wa_arqtxt.


DATA: BEGIN OF wa_datxls,
         line            LIKE sy-tabix,              "linha da planilha
         lifnr           LIKE rf02k-lifnr,           "Fornecedor
         bukrs(60)       TYPE c,                     "Empresa
         ekorg           LIKE rf02k-ekorg,           "Org de compras
         ktokk           LIKE rf02k-ktokk,           "Grp de contas
         title_medi      LIKE sza1_d0100-title_medi, "Forma de tratament
         name1           LIKE addr1_data-name1,      "Descrição
         sortl           LIKE addr1_data-sort1,      "Termo de pesquisa
         street          LIKE addr1_data-street,     "Rua
         house_num1      LIKE addr1_data-house_num1, "Numero
         city2           LIKE addr1_data-city2,      "Bairro
         post_code1      LIKE addr1_data-post_code1, "Codigo postal
         city1           LIKE addr1_data-city1,      "Cidade
         country         LIKE addr1_data-country,    "País
         region          LIKE addr1_data-region,     "Estado
         po_box          LIKE addr1_data-po_box,     "Caixa postal
         post_code2      LIKE addr1_data-post_code2, "Código cx postal
         langu           LIKE addr1_data-langu,      "Idioma
         tel_number      LIKE sza1_d0100-tel_number, "Telefone
         tel_extens      LIKE sza1_d0100-tel_extens, "Ramal
         mob_number      LIKE sza1_d0100-mob_number, "Celular
         fax_number      LIKE sza1_d0100-fax_number, "Fax
         fax_extens      LIKE sza1_d0100-fax_extens, "Ramal de Fax
         deflt_comm      LIKE addr1_data-deflt_comm, "Meio d'comunicação
         remark          LIKE addr1_data-remark,     "Obs sob endereço
         kunnr           LIKE lfa1-kunnr,            "Id como cliente
         stcd1           LIKE lfa1-stcd1,            "CNPJ
         stkzn           LIKE lfa1-stkzn,            "Flag pessoa física
         stcd2           LIKE lfa1-stcd2,            "CPF
         stcd3           LIKE lfa1-stcd3,            "Inscrição Estadual
         stcd4           LIKE lfa1-stcd4,            "Inscrição Municipa
         stenr           LIKE lfa1-stenr,            "Código do PIS
         banks           LIKE lfbk-banks,            "Cod país do banco
         bankl(250)      TYPE c,                     "Chave do banco
         bankn(250)      TYPE c,                     "Nro cta bancária
         bkont(60)       TYPE c,                     "DV da Agência
         bvtyp(100)      TYPE c,                     "Seq Banco/Parceiro
         namev           LIKE knvk-namev,            "Nome de contato
         name2           LIKE knvk-name1,            "descrição contato
         abtnr           LIKE knvk-abtnr,            "Depto do contato
         pafkt           LIKE knvk-pafkt,            "Função do contato
         akont           LIKE lfb1-akont,            "Cta reconciliação
         zuawa           LIKE lfb1-zuawa,            "Chave ordenação
         fdgrv           LIKE lfb1-fdgrv,            "Grp administração
         altkn           LIKE lfb1-altkn,            "Id no legado
         zterm           LIKE lfb1-zterm,            "Chv cond pagamento
         zwels           LIKE lfb1-zwels,            "Forma de pagamento
         zgrup           LIKE lfb1-zgrup,            "Chv de agrupamento
         qland           LIKE lfb1-qland,            "Id país imp retido
         waers           LIKE lfm1-waers,            "Moeda do pedido
         smtp_addr       LIKE sza1_d0100-smtp_addr,  "Email
         eikto           LIKE lfb1-eikto,            "UF RG
         zsabe           LIKE lfb1-zsabe,            "UF Habilitação
      END   OF wa_datxls.

DATA: BEGIN OF wa_carga,
         lifnr           TYPE lfa1-lifnr,
      END   OF wa_carga.

DATA: BEGIN OF wa_bankl,
        bankl            LIKE lfbk-bankl,
      END   OF wa_bankl.

DATA: BEGIN OF wa_bankn,
        bankn            LIKE lfbk-bankn,
      END   OF wa_bankn.

DATA: BEGIN OF wa_bkont,
        bkont            LIKE lfbk-bkont,
      END   OF wa_bkont.

DATA: BEGIN OF wa_bvtyp,
        bvtyp            LIKE lfbk-bvtyp,
      END   OF wa_bvtyp.

DATA: BEGIN OF wa_empresa,
        bukrs            LIKE knb1-bukrs,
      END   OF wa_empresa.

DATA: BEGIN OF wa_lfbk,
         bankl           LIKE lfbk-bankl,
         bankn           LIKE lfbk-bankn,
         bkont           LIKE lfbk-bkont,
         bvtyp           LIKE lfbk-bvtyp,
      END   OF wa_lfbk.

DATA: wa_regcity         LIKE j_1btreg_city,
      wa_fields          TYPE sydes_nameinfo,
      it_empresa         LIKE STANDARD TABLE OF wa_empresa,
      it_bankl           LIKE STANDARD TABLE OF wa_bankl,
      it_bankn           LIKE STANDARD TABLE OF wa_bankn,
      it_bkont           LIKE STANDARD TABLE OF wa_bkont,
      it_bvtyp           LIKE STANDARD TABLE OF wa_bvtyp,

      it_regcity         LIKE STANDARD TABLE OF wa_regcity,
      it_arqtxt          LIKE STANDARD TABLE OF wa_arqtxt,
      it_datxls          LIKE STANDARD TABLE OF wa_datxls,
      it_carga           LIKE STANDARD TABLE OF wa_carga,
      it_lfbk            LIKE STANDARD TABLE OF wa_lfbk,
      it_fields          TYPE sydes_desc.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN COMMENT /1(83) text-cm1.
SELECTION-SCREEN COMMENT /1(83) text-cm2.

SELECTION-SCREEN BEGIN OF TABBED BLOCK blessed FOR 7 LINES.
SELECTION-SCREEN:  TAB (20) tab01 USER-COMMAND fore,
                   TAB (30) tab02 USER-COMMAND back.
SELECTION-SCREEN END   OF BLOCK blessed.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-s03.
PARAMETERS: p_nline  LIKE sy-index        OBLIGATORY.  "Nro aprox Linhas
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF SCREEN 100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-s02.
PARAMETERS  p_cami   LIKE rlgrap-filename.  "Arq Excel/TXT
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-s04.
PARAMETERS: p_optxt  RADIOBUTTON GROUP arq DEFAULT 'X',
            p_opxls  RADIOBUTTON GROUP arq.
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN END   OF SCREEN 100.

SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-s01.
PARAMETERS: p_path   LIKE rlgrap-filename MODIF ID pat,
            p_entr   LIKE rlgrap-filename.  "Arq APLIC.SERVER
SELECTION-SCREEN END   OF BLOCK b0.
SELECTION-SCREEN END   OF SCREEN 200.

INCLUDE zbci001.
*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF ( sy-dynnr EQ 1000 ).
    CASE sy-ucomm.
      WHEN 'FORE'. blessed-dynnr = 100.
      WHEN 'BACK'. blessed-dynnr = 200.
    ENDCASE.
  ENDIF.

  check_modo_bi.
*----------------------------------------------------------------------*
* Event at selection-Screen on value-request
*----------------------------------------------------------------------*
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
* Event at selection-Screen
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON p_cami.

  CHECK ( sy-batch NE 'X' ) AND ( p_cami IS INITIAL ).
  MESSAGE e000 WITH 'Path/Nome do arq. entrada é campo obrigatório!'.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
START-OF-SELECTION.

  PERFORM f_processa_planilha.
  SORT it_datxls BY line.

  IF NOT it_datxls[] IS INITIAL.
    PERFORM f_vendor_create.
  ELSE.
    MESSAGE w000 WITH text-m04.
  ENDIF.

*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
INITIALIZATION.
  LOOP AT SCREEN.
    IF ( screen-group1 EQ 'PAT' ).
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  tab01             = 'Processamento Aberto'.
  tab02             = 'Processamento em Background'.
  blessed-prog      =  sy-repid.
  blessed-dynnr     = 200.
  blessed-activetab = 'BACK'.

  p_path            = c_path.



  SET TITLEBAR 'TITLE01'.
  IF p_nline IS INITIAL.
    p_nline = 9999.
  ENDIF.

  ini_modo_bi.

*----------------------------------------------------------------------*
* Event end-of-selection
*----------------------------------------------------------------------*
*
END-OF-SELECTION.

*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
TOP-OF-PAGE.

  ULINE.
  WRITE: /01 sy-vline,
          02 'Maggi - Projeto Crescer',
          35 'Log de erros de importação - Cargas de Fornecedor',
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
          30 p_cami,
         150 sy-vline.
  WRITE: /01 sy-vline,
          03 'Nro de linhas do arquivo :',
          30 p_nline,
          99 'Usuário:',
         108 sy-uname,
         119 'Programa:',
         129 sy-repid,
         150 sy-vline.
  ULINE.
  WRITE: /01 sy-vline,
          05 'Linha do Excel',
          23 'Emp',
          28 'Nome do Fornecedor',
          63 'Mensagem do log',
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
* Carrega planilha com dados de clientes
*

  DATA:   vl_subrc LIKE sy-subrc,
          vl_cami  TYPE string,
          vl_line  LIKE sy-tabix.



  IF ( p_optxt EQ 'X' ).
*
* Carrega arquivo texto com dados de clientes
*
    CLEAR   it_arqtxt.
    REFRESH it_arqtxt.

    vl_cami = p_cami.

    IF ( sy-batch EQ 'X' ).

      IF ( p_entr IS INITIAL ).
        MESSAGE e000 WITH
        'Nome do arquivo de entrada é campo obrigatório!'.
      ENDIF.

*> Transporta o conteúdo do PATH informado
      CONCATENATE p_path p_entr INTO vg_filename.

      CALL FUNCTION 'Z_BC_OPENDATASET_WITH_MODE'
        EXPORTING
          filename    = vg_filename
        TABLES
          table       = it_arqtxt
        EXCEPTIONS
          open_error  = 1
          read_error  = 2
          write_error = 3
          close_error = 4
          OTHERS      = 5.

      IF ( sy-subrc NE 0 ).
        CALL FUNCTION 'CUTC_GET_MESSAGE'
          EXPORTING
            msg_id      = sy-msgid
            msg_no      = sy-msgno
            msg_arg1    = sy-msgv1
            msg_arg2    = sy-msgv2
            msg_arg3    = sy-msgv3
            msg_arg4    = sy-msgv4
            language    = sy-langu
          IMPORTING
            raw_message = v_mess_tab.

        PERFORM f_imprime_erros  USING v_mess_tab.
        CLEAR v_mess_tab.
      ENDIF.

    ELSE.

      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = vl_cami
        TABLES
          data_tab                = it_arqtxt
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
    ENDIF.
    REFRESH it_datxls.

    CLEAR vl_line.
    LOOP AT it_arqtxt INTO wa_arqtxt.
      CLEAR wa_datxls.
      MOVE-CORRESPONDING wa_arqtxt TO wa_datxls.
      wa_datxls-line = vl_line = vl_line + 1.
      APPEND wa_datxls TO it_datxls.
    ENDLOOP.

  ELSE.

    CLEAR   t_planilha.
    REFRESH t_planilha.
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
      MESSAGE e000 WITH text-m02.
    ENDIF.

    SORT t_planilha BY row col.
    REFRESH it_datxls.

*> Move valores das células
    LOOP AT t_planilha.

      ON CHANGE OF t_planilha-row.
        v_ctrlcol1 = t_planilha-value.
      ENDON.
*> Elimina linhas de cabeçalho e texto da tabela interna.
      IF v_ctrlcol1 EQ 'CAB'.
        DELETE t_planilha.
        CONTINUE.
      ENDIF.

      AT NEW row.
        CLEAR wa_datxls.
        wa_datxls-line = t_planilha-row.
      ENDAT.


      CASE t_planilha-col.
        WHEN  2. wa_datxls-lifnr      = t_planilha-value.
        WHEN  3. wa_datxls-bukrs      = t_planilha-value.
        WHEN  4. wa_datxls-ekorg      = t_planilha-value.
        WHEN  5. wa_datxls-ktokk      = t_planilha-value.
        WHEN  6. wa_datxls-title_medi = t_planilha-value.
        WHEN  7. wa_datxls-name1      = t_planilha-value.
        WHEN  8. wa_datxls-sortl      = t_planilha-value.
        WHEN  9. wa_datxls-street     = t_planilha-value.
        WHEN 10. wa_datxls-house_num1 = t_planilha-value.
        WHEN 11. wa_datxls-city2      = t_planilha-value.
        WHEN 12. wa_datxls-post_code1 = t_planilha-value.
        WHEN 13. wa_datxls-city1      = t_planilha-value.
        WHEN 14. wa_datxls-country    = t_planilha-value.
        WHEN 15. wa_datxls-region     = t_planilha-value.
        WHEN 16. wa_datxls-po_box     = t_planilha-value.
        WHEN 17. wa_datxls-post_code2 = t_planilha-value.
        WHEN 18. wa_datxls-langu      = t_planilha-value.
        WHEN 19. wa_datxls-tel_number = t_planilha-value.
        WHEN 20. wa_datxls-tel_extens = t_planilha-value.
        WHEN 21. wa_datxls-mob_number = t_planilha-value.
        WHEN 22. wa_datxls-fax_number = t_planilha-value.
        WHEN 23. wa_datxls-fax_extens = t_planilha-value.
        WHEN 24. wa_datxls-deflt_comm = t_planilha-value.
        WHEN 25. wa_datxls-remark     = t_planilha-value.
        WHEN 26. wa_datxls-kunnr      = t_planilha-value.
        WHEN 27. wa_datxls-stcd1      = t_planilha-value.
        WHEN 28. wa_datxls-stkzn      = t_planilha-value.
        WHEN 29. wa_datxls-stcd2      = t_planilha-value.
        WHEN 30. wa_datxls-stcd3      = t_planilha-value.
        WHEN 31. wa_datxls-stcd4      = t_planilha-value.
        WHEN 32. wa_datxls-stenr      = t_planilha-value.
        WHEN 33. wa_datxls-banks      = t_planilha-value.
        WHEN 34. wa_datxls-bankl      = t_planilha-value.
        WHEN 35. wa_datxls-bankn      = t_planilha-value.
        WHEN 36. wa_datxls-bkont      = t_planilha-value.
        WHEN 37. wa_datxls-bvtyp      = t_planilha-value.
        WHEN 38. wa_datxls-namev      = t_planilha-value.
        WHEN 39. wa_datxls-name2      = t_planilha-value.
        WHEN 40. wa_datxls-abtnr      = t_planilha-value.
        WHEN 41. wa_datxls-pafkt      = t_planilha-value.
        WHEN 42. wa_datxls-akont      = t_planilha-value.
        WHEN 43. wa_datxls-zuawa      = t_planilha-value.
        WHEN 44. wa_datxls-fdgrv      = t_planilha-value.
        WHEN 45. wa_datxls-altkn      = t_planilha-value.
        WHEN 46. wa_datxls-zterm      = t_planilha-value.
        WHEN 47. wa_datxls-zwels      = t_planilha-value.
        WHEN 48. wa_datxls-zgrup      = t_planilha-value.
        WHEN 49. wa_datxls-qland      = t_planilha-value.
        WHEN 50. wa_datxls-waers      = t_planilha-value.
        WHEN 51. wa_datxls-smtp_addr  = t_planilha-value.
        WHEN 52. wa_datxls-eikto      = t_planilha-value.
        WHEN 53. wa_datxls-zsabe      = t_planilha-value.
        WHEN OTHERS.
      ENDCASE.

      AT END OF row.
        APPEND wa_datxls TO it_datxls.
      ENDAT.

    ENDLOOP.
  ENDIF.
ENDFORM.                    "f_processa_planilha

*&---------------------------------------------------------------------*
*&      Form  f_vendor_create
*&---------------------------------------------------------------------*
FORM f_vendor_create .

  SELECT * FROM j_1btreg_city
           INTO TABLE it_regcity
            FOR ALL ENTRIES IN it_datxls
          WHERE ( country    EQ it_datxls-country    )
            AND ( region     EQ it_datxls-region     )
            AND ( pstcd_from LE it_datxls-post_code1 )
            AND ( pstcd_to   GE it_datxls-post_code1 ).


  SORT it_regcity BY country region pstcd_from pstcd_to.

  LOOP AT it_datxls INTO wa_datxls.

    REFRESH: t_messtab, t_bdcdata.
    CLEAR vg_lifnr.

*> A It controla abaixo controla o nro de vezes que o mesmo Id será
*> usado para ampliação de empresas.
    REFRESH it_empresa.
    SPLIT wa_datxls-bukrs AT '/' INTO TABLE it_empresa.
    READ TABLE it_empresa INTO wa_empresa INDEX 1.
    vg_bukrs = wa_empresa-bukrs.

    PERFORM f_bdc_field USING: 'X' 'SAPMF02K'      '0100',
                               ' ' 'BDC_OKCODE'    '/00',
                               ' ' 'RF02K-LIFNR'   wa_datxls-lifnr,
                               ' ' 'RF02K-BUKRS'   vg_bukrs,
                               ' ' 'RF02K-EKORG'   wa_datxls-ekorg,
                               ' ' 'RF02K-KTOKK'   wa_datxls-ktokk,
                               ' ' 'USE_ZAV'       'X'.

    PERFORM f_bdc_field USING: 'X' 'SAPMF02K'      '0111',
                 ' ' 'BDC_OKCODE'                  '=VW',
                 ' ' 'SZA1_D0100-TITLE_MEDI'       wa_datxls-title_medi,
                 ' ' 'ADDR1_DATA-NAME1'            wa_datxls-name1,
                 ' ' 'ADDR1_DATA-SORT1'            wa_datxls-sortl,
                 ' ' 'ADDR1_DATA-STREET'           wa_datxls-street,
                 ' ' 'ADDR1_DATA-HOUSE_NUM1'       wa_datxls-house_num1,
                 ' ' 'ADDR1_DATA-CITY2'            wa_datxls-city2,
                 ' ' 'ADDR1_DATA-POST_CODE1'       wa_datxls-post_code1,
                 ' ' 'ADDR1_DATA-CITY1'            wa_datxls-city1,
                 ' ' 'ADDR1_DATA-COUNTRY'          wa_datxls-country,
                 ' ' 'ADDR1_DATA-REGION'           wa_datxls-region,
                 ' ' 'ADDR1_DATA-PO_BOX'           wa_datxls-po_box,
                 ' ' 'ADDR1_DATA-POST_CODE2'       wa_datxls-post_code2,
                 ' ' 'ADDR1_DATA-LANGU'            wa_datxls-langu,
                 ' ' 'SZA1_D0100-TEL_NUMBER'       wa_datxls-tel_number,
                 ' ' 'SZA1_D0100-TEL_EXTENS'       wa_datxls-tel_extens,
                 ' ' 'SZA1_D0100-MOB_NUMBER'       wa_datxls-mob_number,
                 ' ' 'SZA1_D0100-FAX_NUMBER'       wa_datxls-fax_number,
                 ' ' 'SZA1_D0100-FAX_EXTENS'       wa_datxls-fax_extens,
                 ' ' 'SZA1_D0100-SMTP_ADDR'        wa_datxls-smtp_addr,
                 ' ' 'ADDR1_DATA-DEFLT_COMM'       wa_datxls-deflt_comm,
                 ' ' 'ADDR1_DATA-REMARK'           wa_datxls-remark.

    LOOP AT it_regcity INTO wa_regcity
                     WHERE ( country    EQ wa_datxls-country    )
                       AND ( region     EQ wa_datxls-region     )
                       AND ( pstcd_from LE wa_datxls-post_code1 )
                       AND ( pstcd_to   GE wa_datxls-post_code1 ).

      PERFORM f_bdc_field USING
                 ' ' 'ADDR1_DATA-TAXJURCODE'      wa_regcity-taxjurcode.
      EXIT.
    ENDLOOP.

    PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0120',
                               ' ' 'BDC_OKCODE'     '=VW',
                               ' ' 'LFA1-KUNNR'     wa_datxls-kunnr.

    IF ( 'ZFNF ZPRF ZFEX ZFUN ZFFF ZMOT' CS wa_datxls-ktokk ).
      PERFORM f_bdc_field USING: ' ' 'LFA1-STCD2'   wa_datxls-stcd2,
                                 ' ' 'LFA1-STKZN'   wa_datxls-stkzn.
    ELSE.
      PERFORM f_bdc_field USING  ' ' 'LFA1-STCD1'   wa_datxls-stcd1.
    ENDIF.

    PERFORM f_bdc_field USING: ' ' 'LFA1-STCD3'     wa_datxls-stcd3,
                               ' ' 'LFA1-STCD4'     wa_datxls-stcd4,
                               ' ' 'LFA1-STENR'     wa_datxls-stenr.

*> Controle das contas do fornecedor: é usado um controle no arquivo
*> XLS/TXT com barras afim de se criar a tabela de contas (LFBK)
    REFRESH: it_lfbk, it_bankl, it_bankn, it_bkont, it_bvtyp.

    SPLIT wa_datxls-bankl AT '/' INTO TABLE it_bankl.
    SPLIT wa_datxls-bankn AT '/' INTO TABLE it_bankn.
    SPLIT wa_datxls-bkont AT '/' INTO TABLE it_bkont.
    SPLIT wa_datxls-bvtyp AT '/' INTO TABLE it_bvtyp.
    DO.
      READ TABLE it_bankl INTO wa_bankl INDEX sy-index.
      IF ( sy-subrc NE 0 ). EXIT. ENDIF.

      READ TABLE it_bankn INTO wa_bankn INDEX sy-index.
      READ TABLE it_bkont INTO wa_bkont INDEX sy-index.
      READ TABLE it_bvtyp INTO wa_bvtyp INDEX sy-index.
      CLEAR wa_lfbk.
      wa_lfbk-bankl = wa_bankl-bankl.
      wa_lfbk-bankn = wa_bankn-bankn.
      wa_lfbk-bkont = wa_bkont-bkont.
      wa_lfbk-bvtyp = wa_bvtyp-bvtyp.
      APPEND wa_lfbk TO it_lfbk.
    ENDDO.

*> A tabela it_lfbk contém dados bancários somente do fornec. corrente
    DESCRIBE TABLE it_lfbk LINES vg_totlines.
    DESCRIBE FIELD wa_lfbk INTO it_fields.
    DELETE it_fields-names WHERE ( name    EQ sy-repid )
                              OR ( name(4) EQ 'LFBK'   ).
    CLEAR vg_index.


    IF NOT it_lfbk[] IS INITIAL.

*> Monta tela do SHDB
      LOOP AT it_lfbk INTO wa_lfbk.
        IF ( vg_index EQ 5 ) AND ( vg_index LT vg_totlines ).
*> Executar o OK_CODE = Paginar, ao chegar ao limite de linhas do ENJOY
          vg_totlines = vg_totlines - vg_index.
          CLEAR vg_index.
          PERFORM f_bdc_field USING  ' ' 'BDC_OKCODE' '=P+'.
        ENDIF.

*> Após a quebra de página do ENJOY, reabrir uma nova tela de entrada
        IF ( vg_index IS INITIAL ).
          PERFORM f_bdc_field USING: 'X' 'SAPMF02K'   '0130'.
        ENDIF.
        vg_index    = vg_index    + 1.

        CONCATENATE 'LFBK-BANKS(' vg_index ')' INTO vg_field_shdb.
        PERFORM f_bdc_field   USING ' ' vg_field_shdb wa_datxls-banks.
*> Mapear os valores encontrados para esta linha.
        LOOP AT it_fields-names INTO wa_fields.
          CONCATENATE 'WA_LFBK-' wa_fields-name INTO vg_field_value.
          ASSIGN (vg_field_value) TO <fs_value>.
          CONCATENATE 'LFBK-'    wa_fields-name '(' vg_index ')'
                                                INTO vg_field_shdb.
          PERFORM f_bdc_field USING ' ' vg_field_shdb <fs_value>.
        ENDLOOP.

        AT LAST.
          PERFORM f_bdc_field USING ' ' 'BDC_OKCODE'  '=VW'.
        ENDAT.
      ENDLOOP.

    ELSE.

      PERFORM f_bdc_field USING: 'X' 'SAPMF02K'   '0130' ,
                                 ' ' 'BDC_OKCODE'  '=VW'.

    ENDIF.


    PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0380',
                               ' ' 'BDC_OKCODE'     '=VW',
                               ' ' 'KNVK-NAMEV(01)' wa_datxls-namev,
                               ' ' 'KNVK-NAME1(01)' wa_datxls-name1,
                               ' ' 'KNVK-ABTNR(01)' wa_datxls-abtnr,
                               ' ' 'KNVK-PAFKT(01)' wa_datxls-pafkt.


*>  Telas referentes a visão de empresa

    LOOP AT it_empresa INTO wa_empresa.

      IF ( wa_empresa-bukrs NE vg_bukrs ).
        IF ( vg_lifnr IS INITIAL ).
          EXIT.
        ENDIF.
        REFRESH: t_messtab, t_bdcdata.

        PERFORM f_bdc_field USING: 'X' 'SAPMF02K'      '0100',
                                   ' ' 'BDC_OKCODE'    '/00',
                                   ' ' 'RF02K-LIFNR'   vg_lifnr,
                                 ' ' 'RF02K-BUKRS'   wa_empresa-bukrs,
                                  ' ' 'RF02K-EKORG'   wa_datxls-ekorg,
                                  ' ' 'RF02K-KTOKK'   wa_datxls-ktokk,
                                   ' ' 'USE_ZAV'       'X'.

      ENDIF.

      PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0210',
                                 ' ' 'BDC_OKCODE'     '=VW',
                                 ' ' 'LFB1-AKONT'     wa_datxls-akont,
                                 ' ' 'LFB1-ZUAWA'     wa_datxls-zuawa,
                                 ' ' 'LFB1-FDGRV'     wa_datxls-fdgrv,
                                 ' ' 'LFB1-ALTKN'     wa_datxls-altkn.

      PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0215',
                                 ' ' 'BDC_OKCODE'     '=VW',
                                 ' ' 'LFB1-ZTERM'     wa_datxls-zterm,
                                 ' ' 'LFB1-ZGRUP'     wa_datxls-zgrup,
                                 ' ' 'LFB1-ZWELS'     wa_datxls-zwels.

      PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0220',
                                 ' ' 'BDC_OKCODE'     '=VW',
                                 ' ' 'LFB1-EIKTO'     wa_datxls-eikto,
                                 ' ' 'LFB1-ZSABE'     wa_datxls-zsabe.

      PERFORM f_bdc_field USING: 'X' 'SAPMF02K'       '0610',
                                 ' ' 'LFB1-QLAND'     wa_datxls-qland.

      IF ( wa_empresa-bukrs NE vg_bukrs ).
        PERFORM f_bdc_field USING  ' ' 'BDC_OKCODE'   '=UPDA'.
      ELSE.
        PERFORM f_bdc_field USING  ' ' 'BDC_OKCODE'   '=VW'.

        PERFORM f_bdc_field USING: 'X' 'SAPMF02K'     '0310',
                                   ' ' 'BDC_OKCODE'   '=UPDA',
                                   ' ' 'LFM1-WAERS'   wa_datxls-waers.
      ENDIF.


* ---> S4 Migration - 21/06/2023 - JS
*      CALL TRANSACTION 'XK01' USING t_bdcdata
*                               MODE p_mod1
*                             UPDATE 'S'
*                      MESSAGES INTO t_messtab.

      DATA: lt_bdc    TYPE bdcdata_tab,
            lt_bdcmsg TYPE tab_bdcmsgcoll,
            wa_lfa1   type lfa1.

      DATA: lo_migbp TYPE REF TO /mignow/cl_migbp.

      lt_bdc = CONV #( t_bdcdata[] ).

      CREATE OBJECT lo_migbp
        EXPORTING
          im_test    = abap_false
          im_tcode   = 'BP'
          it_bdcdata = lt_bdc.

      CALL METHOD lo_migbp->mt_bp_process_old_shdb(
        CHANGING
          ct_bdcmsg = lt_bdcmsg ).

      CALL METHOD lo_migbp->mt_bp_process_data( CHANGING ct_bdcmsg = lt_bdcmsg ).

      t_messtab = CONV #( lt_bdcmsg[] ).
* <--- S4 Migration - 21/06/2023 - JS

      IF sy-subrc EQ 0.

*      data: wa_tmp like zbccarga_tmp.
        IF ( vg_lifnr IS INITIAL ).
          IMPORT tmp TO vg_lifnr FROM SHARED BUFFER indx(hk) ID 'TES'.
          DELETE FROM SHARED BUFFER indx(hk) ID 'TES'.

          vg_lifnr_aux = vg_lifnr.



          IF ( NOT vg_lifnr IS INITIAL ).
            ASSIGN icon_checked TO <icone>.
            CONCATENATE 'Fornecedor' vg_lifnr_aux 'criado com sucesso!'
                                     INTO v_mess_tab SEPARATED BY space.
            PERFORM f_imprime_erros  USING v_mess_tab.
            CLEAR v_mess_tab.
          ENDIF.
          CLEAR vg_lifnr_aux.
        ENDIF.
      ENDIF.
*      select single for update *
*             from zbccarga_tmp
*            where ( carga eq 'FOR' ).
*
*      if ( sy-subrc eq 0 ).
*        vg_lifnr = zbccarga_tmp-registro(10).
*        delete zbccarga_tmp.
*      else.
*        clear vg_lifnr.
*      endif.

      LOOP AT t_messtab INTO DATA(WA_MESSTAB).
        IF 'WAE' CS wa_messtab-msgtyp.
          ASSIGN icon_incomplete TO <icone>.
        ELSEIF wa_messtab-msgtyp EQ 'S'.
          ASSIGN icon_checked TO <icone>.
        ELSEIF wa_messtab-msgtyp EQ 'I'.
          ASSIGN icon_failure TO <icone>.
        ENDIF.

        sy-msgid = wa_messtab-msgid.
        sy-msgno = wa_messtab-msgnr.
        sy-msgv1 = wa_messtab-msgv1.
        sy-msgv2 = wa_messtab-msgv2.
        sy-msgv3 = wa_messtab-msgv3.
        sy-msgv4 = wa_messtab-msgv4.

        CALL FUNCTION 'CUTC_GET_MESSAGE'
          EXPORTING
            msg_id      = sy-msgid
            msg_no      = sy-msgno
            msg_arg1    = sy-msgv1
            msg_arg2    = sy-msgv2
            msg_arg3    = sy-msgv3
            msg_arg4    = sy-msgv4
            language    = sy-langu
          IMPORTING
            raw_message = v_mess_tab.

        PERFORM f_imprime_erros  USING v_mess_tab.
        CLEAR v_mess_tab.
      ENDLOOP.

    ENDLOOP.
    ULINE.
  ENDLOOP.


ENDFORM.                    " f_vendor_create


*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING    value(p_flag)
                          value(p_fnam)
                          value(p_fval).

  CLEAR t_bdcdata.
  IF NOT p_flag IS INITIAL.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
    t_bdcdata-dynbegin = 'X'.
  ELSE.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  ENDIF.
  APPEND t_bdcdata.

ENDFORM.                    "f_bdc_field


*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
FORM f_imprime_erros USING    p_message.
*  write: /01 sy-vline,
*          03 <icone> as icon,
*          08(05) wa_datxls-line,
*          14 vg_lifnr,
*          23 wa_empresa-bukrs,
*          28 wa_datxls-name1(32),
*          63 p_message,
*         150 sy-vline.
  WRITE: /01 sy-vline,
          03 <icone> AS ICON,
          08(05) wa_datxls-line,
          15 wa_empresa-bukrs,
          23 wa_datxls-name1,
          63 p_message,
         150 sy-vline.

ENDFORM.                    " f_imprime_erros
