*&--------------------------------------------------------------------&*
*&                     Programa Módulo - LES                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 14/11/2024                                              &*
*& Descrição: ZLES0192 - Carregar dados de NF via planilha de carga   &*
*& Transação: ZLES0195                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  : Ninjas Evolution                                        &*
*& Código Espec.Funcional/Técnica: 156544                             &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
*&---------------------------------------------------------------------*
*& Report  ZLESR0164
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zlesr0164 NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
*                                 type-pools                           *
*----------------------------------------------------------------------*
TYPE-POOLS icon.

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_line,
         line TYPE char600,
       END   OF type_line.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_line TYPE TABLE OF type_line.

DATA:
* Tabela de mapeamento de tela da transação do BI
  t_bdc     TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  v_mode    TYPE c VALUE 'N',
  t_messtab TYPE TABLE OF bdcmsgcoll,
  w_messtab TYPE bdcmsgcoll,
  ok_code   TYPE sy-ucomm,
  p_ordem   TYPE i.

TYPES: BEGIN OF tp_messagem.
         INCLUDE STRUCTURE bdcmsgcoll.
TYPES:   icone TYPE c LENGTH 4.
TYPES: END OF tp_messagem.

DATA: t_messtab2 TYPE TABLE OF tp_messagem INITIAL SIZE 0 WITH HEADER LINE,
      w_messtab2 TYPE tp_messagem.

DATA: itab TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE,

      BEGIN OF wa_t0002,
        chave_nfe        TYPE c LENGTH 44,  "Chave NF
        nfe	             TYPE znfe,       "NFe
        bukrs            TYPE bukrs,        "Empresa entrada
        branch           TYPE branch_kk,    "Filial Entrada
        tp_transgenia    TYPE zde_tp_trang, "Transgeníase
        local_descarga   TYPE zde_id_local_entrega, "Local Descarga
        parid            TYPE zparid,
        qtd_descarga     TYPE zde_qtd_descarga, "Quantidade Descarga - US #178837 - MMSILVA - 12.05.2025
        safra            TYPE gjahr, "Safra - US #180339- MMSILVA - 26.05.2025
        cod_material_sap TYPE matnr, "Quantidade Descarga - US #180339- MMSILVA - 26.05.2025
      END OF wa_t0002,

      tl_t0002 LIKE STANDARD TABLE OF wa_t0002.

* US #178837 - MMSILVA - 12.05.2025 - Inicio
DATA: ls_numeric TYPE string.
* US #178837 - MMSILVA - 12.05.2025 - Fim

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_filetxt TYPE char26 VALUE 'Files CSV (*.CSV)|*.CSV|',
           c_inicial TYPE char3  VALUE 'C:\',
           c_x       TYPE char1  VALUE 'X'.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
    PARAMETERS
      p_file   TYPE sdba_a_nam OBLIGATORY DEFAULT 'C:/Dados_NF.CSV'.
  SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

* Ajuda Pesquiza Campo Filename
  PERFORM z_ajuda_filename.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Upload do Arquivo
  PERFORM: z_upload  ,
* Processa Arquivo
           z_processa.

*&---------------------------------------------------------------------*
*&      Form  Z_AJUDA_FILENAME                                         *
*&---------------------------------------------------------------------*
*                      Ajuda Pesquiza Campo Filename                   *
*----------------------------------------------------------------------*
FORM z_ajuda_filename.

  DATA: vl_title   TYPE string,
        vl_filter  TYPE string,
        vl_initial TYPE string,
        vl_rc      TYPE i,
        tl_file    TYPE filetable.

  vl_title   = TEXT-002.
  vl_filter  = c_filetxt.
  vl_initial = c_inicial.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = vl_title
      file_filter             = vl_filter
      initial_directory       = vl_initial
    CHANGING
      file_table              = tl_file
      rc                      = vl_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  READ TABLE tl_file INTO p_file INDEX 1.

ENDFORM.                    " Z_AJUDA_FILENAME


*&---------------------------------------------------------------------*
*&      Form  Z_UPLOAD                                                 *
*&---------------------------------------------------------------------*
*                              Upload Arquivo                          *
*----------------------------------------------------------------------*
FORM z_upload.

  DATA vl_file TYPE string.

  REFRESH t_line.

  vl_file = p_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = vl_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = t_line
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  DELETE t_line INDEX 1.

  IF t_line[] IS INITIAL.
    MESSAGE i836 WITH TEXT-003.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_UPLOAD

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA                                               *
*&---------------------------------------------------------------------*
*                             Processa Arquivo                         *
*----------------------------------------------------------------------*
FORM z_processa.

  DATA: sl_line          TYPE type_line,
        w_zlest0205      TYPE zlest0205,
        it_zlest0205     TYPE TABLE OF zlest0205,
        vg_segmento      TYPE string,
        ln_bukrs(4)      TYPE n,
        lc_ch_referencia TYPE zlest0205-ch_referencia,
        lc_nr_safra      TYPE zlest0205-nr_safra,
        lc_remessa       TYPE zsdt0001-doc_rem,
        lc_material      TYPE zsdt0001-matnr.

  FREE t_messtab2[].

  IF t_line[] IS INITIAL.
    APPEND INITIAL LINE TO t_messtab2 ASSIGNING FIELD-SYMBOL(<fs_mess>).
*  Erro informações não encontradas!
    <fs_mess>-msgtyp = 'E'.
    <fs_mess>-msgv1  = 'Erro informações não encontradas!'.
    <fs_mess>-msgv3  = 'Excel(CSV)'.
    <fs_mess>-icone = icon_alert.
  ENDIF.

  FREE: wa_t0002, tl_t0002.
* Busca info planilha
  LOOP AT t_line INTO sl_line.
    CLEAR wa_t0002.

    SPLIT sl_line-line AT ';' INTO wa_t0002-chave_nfe
                                   wa_t0002-nfe
                                   wa_t0002-bukrs
                                   wa_t0002-branch
                                   wa_t0002-tp_transgenia
                                   wa_t0002-local_descarga
                                   wa_t0002-qtd_descarga "US #178837 - MMSILVA - 12.05.2025
                                   wa_t0002-safra "US #180339- MMSILVA - 26.05.2025
                                   wa_t0002-cod_material_sap. "US #180339- MMSILVA - 26.05.2025
    IF wa_t0002 IS INITIAL.
      CONTINUE.
    ENDIF.

    ln_bukrs = wa_t0002-bukrs.
    wa_t0002-bukrs = ln_bukrs.
    APPEND wa_t0002 TO tl_t0002.
    CLEAR wa_t0002.
    CLEAR sl_line.
  ENDLOOP.

* Verifica erros
  LOOP AT tl_t0002 ASSIGNING FIELD-SYMBOL(<fs_t0002>).
    DATA(lv_line) = sy-tabix.
    APPEND INITIAL LINE TO t_messtab2 ASSIGNING <fs_mess>.

    IF <fs_t0002>-chave_nfe IS INITIAL.
      <fs_mess>-msgtyp = 'E'.
      <fs_mess>-msgv1  = 'Chave NFe campo obrigatório(CSV)!'.
      <fs_mess>-msgv2  = 'Linha:' && lv_line.
      <fs_mess>-msgv3  = 'Excel(CSV)'.
      <fs_mess>-icone = icon_alert.
    ENDIF.

*    IF <fs_t0002>-nfe IS INITIAL.
*      <fs_mess>-msgtyp = 'E'.
*      <fs_mess>-msgv1  = 'NFe campo obrigatório(CSV)!'.
*      <fs_mess>-msgv2  = 'Linha:' && lv_line.
*      <fs_mess>-msgv3  = 'Excel(CSV)'.
*      <fs_mess>-icone = icon_alert.
*    ENDIF.

    IF <fs_t0002>-bukrs IS INITIAL.
      <fs_mess>-msgtyp = 'E'.
      <fs_mess>-msgv1  = 'Empresa campo obrigatório(CSV)!'.
      <fs_mess>-msgv2  = 'Linha:' && lv_line.
      <fs_mess>-msgv3  = 'Excel(CSV)'.
      <fs_mess>-icone = icon_alert.
    ELSE.
      SELECT SINGLE bukrs INTO @DATA(lv_bukrs)
        FROM t001
        WHERE bukrs = @<fs_t0002>-bukrs.
      IF sy-subrc NE 0.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Empresa não encontrada(CSV)!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

    IF <fs_t0002>-branch IS INITIAL.
      <fs_mess>-msgtyp = 'E'.
      <fs_mess>-msgv1  = 'Filial campo obrigatório(CSV)!'.
      <fs_mess>-msgv2  = 'Linha:' && lv_line.
      <fs_mess>-msgv3  = 'Excel(CSV)'.
      <fs_mess>-icone = icon_alert.
    ELSE.
      SELECT SINGLE branch INTO @DATA(lv_branch)
        FROM j_1bbranch
        WHERE branch = @<fs_t0002>-branch.
      IF sy-subrc NE 0.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Filial não encontrada(CSV)!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

    IF <fs_t0002>-branch IS NOT INITIAL AND
       <fs_t0002>-bukrs IS NOT INITIAL.
      SELECT SINGLE branch INTO <fs_t0002>-branch
        FROM j_1bbranch
        WHERE branch = <fs_t0002>-branch
          AND bukrs = <fs_t0002>-bukrs.
      IF sy-subrc NE 0.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Filial não pertence a empresa informada!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

    IF <fs_t0002>-local_descarga  IS NOT INITIAL.
      SELECT SINGLE id_local_entrega INTO <fs_t0002>-local_descarga
        FROM zsdt0001le
        WHERE id_local_entrega = <fs_t0002>-local_descarga.
      IF sy-subrc NE 0.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Local de Descarga não encontrado!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

*   US #178837 - MMSILVA - 12.05.2025 - Inicio
    IF <fs_t0002>-qtd_descarga IS NOT INITIAL.
      ls_numeric = <fs_t0002>-qtd_descarga.
      IF NOT ls_numeric CO '0123456789'.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Inserir apenas números no campo Qtd. Descarga!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
      CLEAR: ls_numeric.
    ENDIF.

    IF <fs_t0002>-cod_material_sap IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = <fs_t0002>-cod_material_sap
        IMPORTING
          output = <fs_t0002>-cod_material_sap.
    ENDIF.

    IF <fs_mess>-msgv1 IS NOT INITIAL.
      CLEAR <fs_t0002>.
    ENDIF.
*   US #178837 - MMSILVA - 12.05.2025 - Fim

  ENDLOOP.

* Busca informações
  LOOP AT tl_t0002 ASSIGNING <fs_t0002>.
*   US #178837 - MMSILVA - 12.05.2025 - Inicio
    CHECK <fs_t0002> IS NOT INITIAL.
*   US #178837 - MMSILVA - 12.05.2025 - Fim
    lv_line = sy-tabix.
    APPEND INITIAL LINE TO t_messtab2 ASSIGNING <fs_mess>.
*    <fs_mess>-msgv1 = abap_false.
    IF <fs_t0002>-chave_nfe IS NOT INITIAL AND
       <fs_t0002>-nfe       IS NOT INITIAL.

* --> US #180339 - MMSILVA - 26.05.2025 - Inicio <--
      SELECT SINGLE * INTO @DATA(ls_zib_nfe_dist_ter)
        FROM zib_nfe_dist_ter
        WHERE chave_nfe = @<fs_t0002>-chave_nfe.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM lfa1
          INTO @DATA(ls_lfa1)
          WHERE stcd1 = @ls_zib_nfe_dist_ter-forne_cnpj.

        "Apenas entrar no PERFORM F_BUSCAR_FATURAMENTO caso o fornecedor seja ZFIC
        IF ls_lfa1-ktokk NE 'ZFIC'.
          w_zlest0205-ch_referencia = ''.
          w_zlest0205-nr_safra      = <fs_t0002>-safra.
          w_zlest0205-matnr         = <fs_t0002>-cod_material_sap.
        ELSE.
* --> US #180339 - MMSILVA - 26.05.2025 - Fim <--
          PERFORM f_buscar_faturamento    USING <fs_t0002>-chave_nfe
                                       CHANGING lc_ch_referencia
                                                lc_nr_safra
                                                lc_remessa
                                                lc_material.
        ENDIF. "US #180339 - MMSILVA - 26.05.2025

        SELECT SINGLE * INTO @DATA(ls_zib_nfe_dist_itm)
          FROM zib_nfe_dist_itm
          WHERE chave_nfe = @<fs_t0002>-chave_nfe.

        DATA(obj) = NEW zcl_fornecedores( ).

        obj->zif_parceiros~set_parceiro_cnpj_cpf_ie(
            EXPORTING
              i_cnpj             = CONV #( ls_zib_nfe_dist_ter-forne_cnpj )
           )->get_id_parceiro( IMPORTING e_parceiro = <fs_t0002>-parid ).

        IF  ls_zib_nfe_dist_itm-prod_und_trib EQ 'TN' OR
            ls_zib_nfe_dist_itm-prod_und_trib EQ 'TO' OR
            ls_zib_nfe_dist_itm-prod_und_trib EQ 'TON'.
          ls_zib_nfe_dist_itm-prod_qtd_comerci = ls_zib_nfe_dist_itm-prod_qtd_trib * 1000. "BUG #178835 - MMSILVA - 13.05.2025 - Alterado de prod_qtd_comerci PARA prod_qtd_trib
        ENDIF.
        w_zlest0205-chave_nfe = <fs_t0002>-chave_nfe.
        w_zlest0205-bukrs = <fs_t0002>-bukrs.
        w_zlest0205-branch = <fs_t0002>-branch.
        w_zlest0205-nfe = <fs_t0002>-nfe.
        w_zlest0205-tp_transgenia = <fs_t0002>-tp_transgenia.
        w_zlest0205-local_descarga = <fs_t0002>-local_descarga.
        w_zlest0205-parid = <fs_t0002>-parid.

        w_zlest0205-peso_fiscal =  ls_zib_nfe_dist_itm-prod_qtd_comerci.
        w_zlest0205-nfnum =  ls_zib_nfe_dist_ter-numero.
        w_zlest0205-series =  ls_zib_nfe_dist_ter-serie.
        w_zlest0205-docdat =  ls_zib_nfe_dist_ter-dt_emissao.
        w_zlest0205-netwr  = ls_zib_nfe_dist_ter-vl_total_fatura.

* ----> US #178837 - MMSILVA - 12.05.2025 - Inicio <----
        IF <fs_t0002>-qtd_descarga IS INITIAL.
          w_zlest0205-peso_subtotal  = ls_zib_nfe_dist_itm-prod_qtd_comerci.
        ELSE.
          w_zlest0205-peso_subtotal  = <fs_t0002>-qtd_descarga.
        ENDIF.
* ----> US #178837 - MMSILVA - 12.05.2025 - Fim <----

        w_zlest0205-cfop  = ls_zib_nfe_dist_itm-prod_cfop.


      ELSE.
*  Erro informações não encontradas!
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Chave Nfe não encontrada na base de dados do SAP!'. "US #180339 - MMSILVA - 26.05.2025
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.

    ENDIF.

    IF ls_lfa1-ktokk = 'ZFIC'. "US #180339 - MMSILVA - 26.05.2025 - Criado condição ZFIC
      IF w_zlest0205-ch_referencia IS INITIAL.
        w_zlest0205-ch_referencia = lc_ch_referencia.
      ENDIF.

      IF w_zlest0205-nr_safra      IS INITIAL.
        w_zlest0205-nr_safra      = lc_nr_safra.
      ENDIF.

      IF w_zlest0205-doc_rem       IS INITIAL.
        w_zlest0205-doc_rem       = lc_remessa.
      ENDIF.

      IF w_zlest0205-matnr         IS INITIAL.
        w_zlest0205-matnr         = lc_material.
      ENDIF.
    ENDIF. "US #180339 - MMSILVA - 26.05.2025 - Criado condição ZFIC

    IF <fs_t0002>-chave_nfe IS INITIAL.
      IF w_zlest0205-bukrs IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Empresa obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-branch IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Filial obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-parid IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Parceiro obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-nr_safra IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Safra obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-matnr   IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Material obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-peso_fiscal IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Peso Fiscal obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-nfnum IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Nota Fiscal obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-series IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Série obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-docdat IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Data Emissão obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-netwr IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Valor da NF obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-peso_subtotal IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Peso Subtotal obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-local_descarga IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Local Descarga obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-cfop IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo CFOP obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

    IF <fs_t0002>-chave_nfe IS NOT INITIAL.
      IF w_zlest0205-bukrs IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Empresa obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-branch IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Filial obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-nr_safra IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Safra obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-matnr   IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Material obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ELSEIF w_zlest0205-local_descarga IS INITIAL.
        <fs_mess>-msgtyp = 'E'.
        <fs_mess>-msgv1  = 'Campo Local Descarga obrigatório!'.
        <fs_mess>-msgv2  = 'Linha:' && lv_line.
        <fs_mess>-msgv3  = 'Excel(CSV)'.
        <fs_mess>-icone = icon_alert.
      ENDIF.
    ENDIF.

    IF <fs_mess>-msgv1 IS INITIAL.
      <fs_mess>-msgtyp = 'S'.
      <fs_mess>-msgv1  = 'Carga realizada com sucesso!' && 'NF:' && w_zlest0205-nfnum.
      <fs_mess>-msgv2  = 'Linha:' && lv_line.
      <fs_mess>-msgv3  = 'Excel(CSV)'.
      <fs_mess>-icone  = icon_hint.
*   US #178837 - MMSILVA - 12.05.2025 - Inicio
    ELSE.
      CLEAR: w_zlest0205.
    ENDIF.


    IF w_zlest0205 IS NOT INITIAL.
      APPEND w_zlest0205 TO it_zlest0205.
    ENDIF.
*   US #178837 - MMSILVA - 12.05.2025 - Fim

    CLEAR w_zlest0205.
  ENDLOOP.

  DELETE it_zlest0205[] WHERE chave_nfe IS INITIAL.

  IF it_zlest0205[] IS NOT INITIAL.
    MODIFY zlest0205 FROM TABLE it_zlest0205[].
    IF sy-subrc IS INITIAL.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

  SORT t_messtab2[] BY msgtyp DESCENDING
                       msgv2  ASCENDING.
  DELETE t_messtab2[] WHERE msgtyp IS INITIAL.

  IF t_messtab2[] IS NOT INITIAL.
    PERFORM mostra_log.
  ENDIF.

ENDFORM.                    " Z_PROCESSA
*&---------------------------------------------------------------------*
*&      Form  MOSTRA_LOG
*&---------------------------------------------------------------------*
*       text
FORM mostra_log .
  CALL SCREEN 0001 STARTING AT 10 05 ENDING AT 130 30.
ENDFORM.                    " MOSTRA_LOG

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_LOSG' ITSELF
CONTROLS: tab_losg TYPE TABLEVIEW USING SCREEN 0001.

*&SPWIZARD: OUTPUT MODULE FOR TC 'TAB_LOSG'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: UPDATE LINES FOR EQUIVALENT SCROLLBAR
MODULE tab_losg_change_tc_attr OUTPUT.
  DESCRIBE TABLE t_messtab2 LINES tab_losg-lines.
ENDMODULE.                    "TAB_LOSG_CHANGE_TC_ATTR OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PFLOG'.
  SET TITLEBAR 'TLLOG'.


ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

  CASE ok_code.
    WHEN 'QUIT'.
      CLEAR ok_code.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0001  INPUT

FORM f_buscar_faturamento    USING p_chave_nfe
                          CHANGING p_ch_referencia
                                   p_nr_safra
                                   p_remessa
                                   p_material.

  DATA: zcl_util      TYPE REF TO zcl_util,
        lc_campos_nfe TYPE zde_campos_nfe,
        lc_filial     TYPE char01,
        lc_matkl      TYPE mara-matkl,
        lc_matnr      TYPE mara-matnr,
        lc_erro       TYPE char01,
        t_active      TYPE TABLE OF j_1bnfe_active,
        w_active      TYPE j_1bnfe_active.

  FREE: p_ch_referencia, p_nr_safra, p_material.

  CREATE OBJECT zcl_util.

  PERFORM f_validar_cnpj_material    USING p_chave_nfe
                                  CHANGING lc_filial
                                           lc_matkl
                                           p_material
                                           lc_erro.

  CHECK lc_filial = abap_true.

  lc_campos_nfe  = zcl_util->get_atributos_nfe( p_chave_nfe ).

  SELECT SINGLE j_1bnfe_active~docnum
    INTO @DATA(_docnum)
    FROM j_1bnfe_active
   INNER JOIN j_1bnfdoc        ON j_1bnfdoc~docnum = j_1bnfe_active~docnum
   WHERE j_1bnfe_active~regio   = @lc_campos_nfe-regio
     AND j_1bnfe_active~nfyear  = @lc_campos_nfe-nfyear
     AND j_1bnfe_active~nfmonth = @lc_campos_nfe-nfmonth
     AND j_1bnfe_active~stcd1   = @lc_campos_nfe-stcd1
     AND j_1bnfe_active~model   = @lc_campos_nfe-model
     AND j_1bnfe_active~serie   = @lc_campos_nfe-serie
     AND j_1bnfe_active~nfnum9  = @lc_campos_nfe-nfnum9
     AND j_1bnfe_active~docnum9 = @lc_campos_nfe-docnum9
     AND j_1bnfe_active~cdv     = @lc_campos_nfe-cdv
     AND j_1bnfdoc~direct       = '2'
     AND j_1bnfdoc~cancel       = @abap_off.

  IF sy-subrc = 0.

    SELECT SINGLE refkey, reftyp
      INTO @DATA(_j_1bnflin)
      FROM j_1bnflin
     WHERE docnum = @_docnum.

    CHECK sy-subrc = 0 AND _j_1bnflin-reftyp = 'BI'.

    SELECT SINGLE vbelv
      INTO @DATA(_vbelv)
      FROM vbfa
     WHERE vbeln = @_j_1bnflin-refkey
       AND vbtyp_v = 'J'.

    CHECK sy-subrc = 0.

    SELECT SINGLE ch_referencia, nr_safra
      INTO @DATA(_zsdt0001)
      FROM zsdt0001
     WHERE doc_rem      = @_vbelv
       AND tp_movimento = 'S'.

    CHECK sy-subrc = 0.

    p_ch_referencia = _zsdt0001-ch_referencia.
    p_nr_safra      = _zsdt0001-nr_safra.
    p_remessa       = _vbelv.

  ELSE.

    APPEND INITIAL LINE TO t_messtab2 ASSIGNING FIELD-SYMBOL(<fs_mess>).
*  Erro informações não encontradas!
    <fs_mess>-msgtyp = 'E'.
    <fs_mess>-msgv1  = 'Chave Nfe não encontradas!'.
    <fs_mess>-icone = icon_alert.

  ENDIF.

ENDFORM.

FORM f_validar_cnpj_material    USING p_chave_nfe
                             CHANGING p_filial
                                      p_matkl
                                      p_matnr
                                      p_erro.

  DATA: lc_matnr     TYPE mara-matnr.

  FREE: p_filial, p_matkl, p_matnr, p_erro.

  SELECT SINGLE forne_cnpj
    INTO @DATA(_forne_cnpj)
    FROM zib_nfe_dist_ter
   WHERE chave_nfe = @p_chave_nfe.

  IF sy-subrc = 0.
    SELECT SINGLE stcd1
      INTO @DATA(_stcd1)
      FROM j_1bbranch
     WHERE stcd1 = @_forne_cnpj.

    IF sy-subrc = 0.
      SELECT SINGLE prod_codigo
        INTO @DATA(_prod_codigo)
        FROM zib_nfe_dist_itm
       WHERE chave_nfe = @p_chave_nfe.

      IF sy-subrc = 0.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = _prod_codigo
          IMPORTING
            output = lc_matnr.

        SELECT SINGLE matkl
          INTO @DATA(_matkl)
          FROM mara
         WHERE matnr = @lc_matnr.

        IF sy-subrc = 0.
          SELECT SINGLE low
            INTO @DATA(_low)
            FROM tvarvc
           WHERE name = 'MAGGI_GR_GRAOS'
             AND low  = @_matkl.

          IF sy-subrc = 0.
            p_filial = abap_true.
            p_matnr  = lc_matnr.
          ELSE.
            p_erro   = abap_true.
            p_matkl  = _matkl.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
