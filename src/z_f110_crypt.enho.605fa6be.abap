"Name: \PR:RFFOBR_U\FO:DME_BRAZIL\SE:END\EI
ENHANCEMENT 0 Z_F110_CRYPT.

CONSTANTS: c_extcom TYPE sxpgcolist-name VALUE 'Z_F110_CRYPT',
           c_oper   TYPE syopsys VALUE 'SunOS'.

DATA: v_dir_input      TYPE sxpgcolist-parameters.  " Input Directory
DATA: t_result         TYPE STANDARD TABLE OF btcxpm.
DATA: w_result         TYPE btcxpm.
DATA: st_zlest0007     TYPE zlest0007.
DATA: t_empresas       TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

DATA: lv_len    TYPE i,
      lv_count  TYPE i,
      lv_inicio TYPE i,
      lv_final  TYPE i.

DATA: hlp_filename_s(255).

* "// Envia dados de Criação do PIX via API do ITAU - US-152654 WBARBOSA 20/01/2025
DATA: is_api TYPE c.

is_api = abap_false.

IF t042z-zlsch EQ abap_true AND "// Forma de Pagamento
  reguh-xvorl NE abap_true. "// Não Enviar Proposta

*"// Valida CNPJ e Usuario
  CALL METHOD zcl_fi_utils=>check_acesso
    EXPORTING
      i_cnpj = j_1bwfield-cgc_number
    RECEIVING
      is_ok  = DATA(is_ok).

  IF is_ok IS NOT INITIAL.
*** US - 178728 - CBRAND - Inicio
** // BBD
    IF t012k-hbkid = 'BBD'.
      TRY.
          LOOP.
            CALL METHOD zcl_fi_utils=>send_pix_bradesco
              EXPORTING
                i_cnpj  = j_1bwfield-cgc_number
                i_regup = regup
                i_reguh = reguh
                i_t012k = t012k.
          ENDLOOP.
        CATCH cx_root INTO DATA(ls_root_bbd).
          MESSAGE 'Houve algum problema na Execução da API PIX BRADESCO!'  TYPE 'S' .
      ENDTRY.
      is_api = abap_true.
    ELSE.
*** US - 178728 - CBRAND - Fim
      TRY.
          LOOP.
            CALL METHOD zcl_fi_utils=>send_pix_itau
              EXPORTING
                i_cnpj  = j_1bwfield-cgc_number
                i_regup = regup
                i_reguh = reguh
                i_t012k = t012k.
          ENDLOOP.
        CATCH cx_root INTO DATA(ls_root).
          MESSAGE 'Houve algum problema na Execução da API PIX ITAU!'  TYPE 'S' .
      ENDTRY.
      is_api = abap_true.
    ENDIF.
  ENDIF.
ENDIF.
* "// Envia dados de Criação do PIX via API do ITAU - US-152654 WBARBOSA 20/01/2025

IF is_api IS INITIAL. "// Não Criptografar os dados quando for PIX - US-152654 WBARBOSA 11/03/2025

  " Empresas que não estão habilitadas para criptografia
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_F110_SEM_CRYP'
    TABLES
      set_values    = t_empresas
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_empresas BY from.

  READ TABLE t_empresas WITH KEY from = reguh-zbukr BINARY SEARCH.
  IF sy-subrc NE 0.
    SELECT SINGLE *
       FROM zlest0007
       INTO st_zlest0007
      WHERE id_interface = sy-repid
        AND id_ctg = 'PROC'
        AND vlde <= sy-datum
        AND vlate >= sy-datum.

    IF sy-subrc = 0 AND  reguh-xvorl NE 'X'. "propostas ALRS.
      IF st_zlest0007-pathunix IS INITIAL. "somente adiciona extensão .gpg no mesmo diretorio
        "Arquivo criptografado no diretorio de SAIDA padrão para FINNET
        CONCATENATE hlp_filename '.gpg' INTO hlp_filename_s.
      ELSE.
        "Pega o nome do arquivo
        lv_len = strlen( hlp_filename ).
        lv_inicio = -1.
        WHILE lv_len > 0.
          ADD 1 TO lv_inicio.
          IF hlp_filename+lv_len(1) = '/'.
            EXIT.
          ENDIF.

          SUBTRACT 1 FROM lv_len.
        ENDWHILE.
        lv_len = strlen( hlp_filename ).
        lv_final  = lv_inicio.
        lv_inicio = lv_len - lv_inicio.
        hlp_filename_s = hlp_filename+lv_inicio(lv_final).
        CONDENSE hlp_filename_s NO-GAPS.

        "Arquivo criptografado no diretorio de SAIDA padrão para FINNET
*          CONCATENATE ST_ZLEST0007-PATHUNIX hlp_filename_s '.gpg' into hlp_filename_s.
        CONCATENATE st_zlest0007-pathunix hlp_filename_s '_' sy-uzeit '.gpg' INTO hlp_filename_s.

      ENDIF.
      "Apaga se existir já com o mesmo nome
      DELETE DATASET hlp_filename_s.
      "comando GPG
      CONCATENATE '--output' hlp_filename_s  '--encrypt --armor -r' st_zlest0007-pathwin '--trust-model always' hlp_filename  INTO v_dir_input SEPARATED BY space.

      REFRESH: t_result.
      CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
        EXPORTING
          commandname                   = c_extcom
          additional_parameters         = v_dir_input
          operatingsystem               = c_oper
        TABLES
          exec_protocol                 = t_result
        EXCEPTIONS
          no_permission                 = 1
          command_not_found             = 2
          parameters_too_long           = 3
          security_risk                 = 4
          wrong_check_call_interface    = 5
          program_start_error           = 6
          program_termination_error     = 7
          x_error                       = 8
          parameter_expected            = 9
          too_many_parameters           = 10
          illegal_command               = 11
          wrong_asynchronous_parameters = 12
          cant_enq_tbtco_entry          = 13
          jobcount_generation_error     = 14
          OTHERS                        = 15.

      IF sy-subrc NE  0 OR t_result[] IS NOT INITIAL. "apaga arquivo original
        DELETE DATASET hlp_filename. "Arquivo Original
        WRITE: / 'Erro ao criptografar arquivo codigo erro', sy-subrc.
        LOOP AT t_result INTO w_result.
          WRITE: / w_result-message.
        ENDLOOP.
        MESSAGE e000(z01) WITH  'Erro ao criptografar arquivo codigo erro'
                                 sy-subrc.
      ENDIF.
      DELETE DATASET hlp_filename. "Arquivo Original
    ENDIF.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
