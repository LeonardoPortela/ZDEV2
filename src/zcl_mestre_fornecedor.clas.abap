class ZCL_MESTRE_FORNECEDOR definition
  public
  final
  create public .

public section.

  data GV_LIFNR type LIFNR .
  data GV_BUKRS type BUKRS .
  class-data LO_DATA type ref to ZCL_MESTRE_FORNECEDOR .
  class-data GS_VMDS_EXTERN type VMDS_EI_MAIN .
  data A_BUKRS type BUKRS .

  methods CONSTRUCTOR
    importing
      !I_LIFNR type LIFNR
      !I_BUKRS type BUKRS .
  class-methods GET_INSTANCE
    returning
      value(LO_DATA) type ref to ZCL_MESTRE_FORNECEDOR .
  methods EXPANDIR_DADOS_FORNECEDOR
    importing
      value(I_LFB1) type LFB1
      value(I_LFA1) type LFA1
      value(I_BUKRS) type BUKRS
    exporting
      value(GS_ERR_MESSAGES) type CVIS_MESSAGE
      value(GS_SUCC_MESSAGES) type CVIS_MESSAGE .
  methods PREPARE_DADOS_FORNECEDOR
    exporting
      !I_LFB1 type LFB1
      !I_LFA1 type LFA1 .
  methods SET_EMPRESA
    exporting
      !I_BUKRS type BUKRS .
  methods AJUSTAR_IMPOSTOS_FORNECEDOR
    importing
      value(I_BUKRS) type BUKRS
      value(I_LIFNR) type LIFNR
    exporting
      value(E_MSG_ERRO) type STRING .
  methods PREPARE_DADOS_IMPOSTOS
    importing
      !I_LFA1 type LFA1
      !T_ZFIT0174 type ZFIT0174_T .
  methods SET_RECUPERA_CONTA
    importing
      !I_BUKRS type BUKRS
      !I_LIFNR type LIFNR
      !I_STCD1 type STCD1
      !I_STCD2 type STCD2
    returning
      value(E_AKONT) type AKONT .
  methods EXECT_BLOQ_FORN
    importing
      value(I_LFA1) type LFA1
      value(I_BUKRS) type BUKRS optional
    exporting
      value(GS_ERR_MESSAGES) type CVIS_MESSAGE
      value(GS_SUCC_MESSAGES) type CVIS_MESSAGE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MESTRE_FORNECEDOR IMPLEMENTATION.


  METHOD ajustar_impostos_fornecedor.

    DATA: gs_vmds_extern   TYPE vmds_ei_main,
          gs_vmds_error    TYPE vmds_ei_main,
          gs_vmds_succ     TYPE vmds_ei_main,
          gv_ktokk         TYPE ktokk,
          gv_ccode         TYPE bukrs,
          gv_akont         TYPE akont,
          gv_name          TYPE name1,
          l_bukrs          TYPE bukrs,
          lv_return        TYPE i,
          t_return         TYPE bapiret2_t,
          t_zfit0174       TYPE zfit0174_t,
          gw_err_messages	 TYPE cvis_message,
          gw_succ_messages TYPE cvis_message.

    FREE: gs_vmds_succ,
          gs_vmds_error,
          e_msg_erro.

    DO 5 TIMES.
      SELECT *
        FROM lfa1
          UP TO 1 ROWS
        INTO @DATA(w_lfa1)
       WHERE lifnr = @i_lifnr.
      ENDSELECT.

      IF sy-subrc = 0.
        EXIT.
      ELSE.
        WAIT UP TO 3 SECONDS.
      ENDIF.
    ENDDO.

    CHECK sy-subrc = 0.
    CHECK w_lfa1-ktokk = 'ZFFF'.

    SELECT *
      FROM zfit0174
      INTO TABLE t_zfit0174
     WHERE bukrs = i_bukrs.

    CHECK sy-subrc = 0.

*------------------------------
*-- Setar empresa
*------------------------------
    me->set_empresa(            IMPORTING i_bukrs    = i_bukrs ).   " Empresa

*------------------------------
*-- Preparar dados cadastro fornecedor.
*------------------------------
    me->prepare_dados_impostos( EXPORTING i_lfa1     = w_lfa1    " Mestre de fornecedores (empresa)
                                          t_zfit0174 = t_zfit0174 ).  " Mestre de fornecedores (parte geral)

    CHECK me->gs_vmds_extern-vendors IS NOT INITIAL.

*------------------------------
*   Initialize all the data
*------------------------------
    vmd_ei_api=>initialize( ).

*------------------------------
* executa bapi
*------------------------------
    CALL METHOD vmd_ei_api=>maintain_bapi
      EXPORTING
        is_master_data           = me->gs_vmds_extern
      IMPORTING
        es_master_data_correct   = gs_vmds_succ
        es_message_correct       = gw_succ_messages
        es_master_data_defective = gs_vmds_error
        es_message_defective     = gw_err_messages.

    IF gw_err_messages-is_error IS NOT INITIAL.
      t_return[] = gw_err_messages-messages[].
      READ TABLE t_return INTO DATA(w_return) INDEX 1.
      e_msg_erro = w_return-message.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDMETHOD.


  method CONSTRUCTOR.

    me->gv_lifnr = i_lifnr.
    me->gv_bukrs  = i_bukrs.

  endmethod.


  METHOD expandir_dados_fornecedor.
    DATA: gs_vmds_extern   TYPE vmds_ei_main,
*          gs_succ_messages TYPE cvis_message,
          gs_vmds_error    TYPE vmds_ei_main,
*          gs_err_messages  TYPE cvis_message,
          gs_vmds_succ     TYPE vmds_ei_main,
          gv_ktokk         TYPE ktokk,
          gv_ccode         TYPE bukrs,
          gv_akont         TYPE akont,
          gv_name          TYPE name1.



    DATA: lv_return TYPE i.
    CLEAR: gs_vmds_succ, gs_vmds_error.

    "Setar empresa
    me->set_empresa(
      IMPORTING
        i_bukrs = i_bukrs    " Empresa
    ).

*   Preparar dados cadastro fornecedor.
    me->prepare_dados_fornecedor(
        IMPORTING
          i_lfb1 =  i_lfb1    " Mestre de fornecedores (empresa)
          i_lfa1 =  i_lfa1   " Mestre de fornecedores (parte geral)
      ).

    IF me->gs_vmds_extern-vendors IS INITIAL.
      EXIT.
    ENDIF.

*   Initialize all the data
    vmd_ei_api=>initialize( ).

*
    CALL METHOD vmd_ei_api=>maintain_bapi
      EXPORTING
        is_master_data           = me->gs_vmds_extern
      IMPORTING
        es_master_data_correct   = gs_vmds_succ
        es_message_correct       = gs_succ_messages
        es_master_data_defective = gs_vmds_error
        es_message_defective     = gs_err_messages.

    IF gs_err_messages-is_error IS INITIAL.
      COMMIT WORK.
*      return = gs_succ_messages.
    ENDIF.


  ENDMETHOD.


  method GET_INSTANCE.

    CREATE OBJECT lo_data
    EXPORTING
      i_lifnr = ''
      i_bukrs = ''.

  endmethod.


  METHOD prepare_dados_fornecedor.

    DATA:
*          gs_vmds_extern   TYPE vmds_ei_main,
      gs_succ_messages TYPE cvis_message,
      gs_vmds_error    TYPE vmds_ei_main,
      gs_err_messages  TYPE cvis_message,
      gs_vmds_succ     TYPE vmds_ei_main,
      gv_lifnr         TYPE lifnr,
      gv_bukrs         TYPE bukrs.


* Declaração de dados locais
    DATA: lt_contacts                   TYPE vmds_ei_contacts_t,
          ls_contacts                   TYPE vmds_ei_contacts,
          lt_vendors                    TYPE vmds_ei_extern_t,
          ls_vendors                    TYPE vmds_ei_extern,
          ls_address                    TYPE cvis_ei_address1,
          lt_company                    TYPE vmds_ei_company_t,
          ls_company                    TYPE vmds_ei_company,
          ls_company_wtax_type          TYPE vmds_ei_company-wtax_type-wtax_type, "Incrementar workearia de imposto.

          ls_company_wtax_type_task     TYPE vmd_ei_wtax_task, "Incrementar workearia de imposto.
          ls_company_wtax_type_data_key TYPE vmds_ei_wtax_key, "Incrementar workearia de imposto.
          ls_company_wtax_type_data     TYPE vmds_ei_wtax_dat, "Incrementar workearia de imposto.
          ls_company_wtax_type_datax    TYPE vmds_ei_wtax_datax, "Incrementar workearia de imposto.
          lt_wtax_type_data             TYPE vmds_ei_wtax_type_t,
          ls_wtax_type_data             TYPE vmds_ei_wtax_type,

          ls_company_data               TYPE vmds_ei_vmd_company,
          ls_purchas_data               TYPE vmds_ei_vmd_purchasing,
          lt_purchasing                 TYPE vmds_ei_purchasing_t,
          ls_purchasing                 TYPE vmds_ei_purchasing,
          lt_purch_func                 TYPE vmds_ei_functions_t,
          ls_purch_func                 TYPE vmds_ei_functions,
          ls_message                    TYPE cvis_message,
          lv_contactid                  TYPE bapicontact_01-contact.

    DATA: ws_lfm1 TYPE lfm1,
          t_lfbw  TYPE TABLE OF lfbw.

    CLEAR: ws_lfm1.
    SELECT SINGLE * FROM lfm1 INTO ws_lfm1 WHERE lifnr EQ i_lfa1-lifnr.
    SELECT * FROM lfbw INTO TABLE t_lfbw WHERE lifnr EQ i_lfa1-lifnr.


    CLEAR gs_vmds_extern.

    ls_vendors-header-object_instance-lifnr   = i_lfa1-lifnr.
    ls_vendors-header-object_task = 'U'.      "Interface externa: código de modificação objeto



* Defina o nome
    ls_address-postal-data-name            = i_lfa1-name1."gv_name. "Nome 1
    ls_address-postal-data-street          = i_lfa1-stras. "Rua
    ls_address-postal-data-postl_cod1      = i_lfa1-pstlz. "Código postal da localidade
    ls_address-postal-data-city            = i_lfa1-mcod3. "Cidade / Local
    ls_address-postal-data-district        = i_lfa1-ort02. "Bairro
    ls_address-postal-data-region          = i_lfa1-regio. "Região (estado federal, estado federado, província, condado)
    ls_address-postal-data-country         = i_lfa1-land1. "Chave do país
    ls_address-postal-data-sort1           = i_lfa1-sortl. "gv_name.   "Termo de pesquisa 1
    ls_address-postal-data-langu           = sy-langu.     "Código de idioma

* Para todos os campos onde o valor foi fornecido, defina o valor do sinalizador também como 'X'
    ls_address-postal-datax-name           = 'X'.
    ls_address-postal-datax-street         = 'X'.
    ls_address-postal-datax-postl_cod1     = 'X'.
    ls_address-postal-datax-city           = 'X'.
    ls_address-postal-datax-district       = 'X'.
    ls_address-postal-datax-region         = 'X'.
    ls_address-postal-datax-sort1          = 'X'.
    ls_address-postal-datax-country        = 'X'.
    ls_address-postal-datax-langu          = 'X'.
    ls_address-task                        = 'X'.

*   Defina o endereço do fornecedor.
    ls_vendors-central_data-address = ls_address.

*-#166636-11.02.2025-#166636-JT-inicio - comentado
*   Definir pessoa de contato
*    REFRESH: lt_contacts[].
*    CLEAR ls_contacts.
*    ls_contacts-task = 'I'. "Representa a criação de uma pessoa de contato
*    ls_contacts-address_type_3-task = 'U'.  "representa a criação de endereço para cp
*    ls_contacts-data_key-parnr = lv_contactid. "número da pessoa de contato
*
**   Defina o nome da pessoa de contato
*    ls_contacts-address_type_3-postal-data-fullname   = i_lfa1-name1.   "Nome completo da pessoa
*    ls_contacts-address_type_3-postal-data-firstname  = i_lfa1-name1.   "1º nome
*    ls_contacts-address_type_3-postal-data-lastname   = i_lfa1-name1.   "Sobrenome
*    APPEND ls_contacts TO lt_contacts.
*
**   Defina os detalhes da pessoa de contato para o fornecedor
*    ls_vendors-central_data-contact-contacts = lt_contacts[].
*-#166636-11.02.2025-#166636-JT-fim - comentado

*   Set the Account Group
    ls_vendors-central_data-central-data-ktokk  = i_lfa1-ktokk. "Grupo de contas do fornecedor
    ls_vendors-central_data-central-data-stcd1  = i_lfa1-stcd1. "Nº ID fiscal 1
    ls_vendors-central_data-central-data-stcd3  = i_lfa1-stcd3. "Nº identificação fiscal 3
    ls_vendors-central_data-central-data-brsch  = i_lfa1-brsch. "Chave do setor industrial
*   Set the DATAX flags
    ls_vendors-central_data-central-datax-ktokk = 'X'.
    ls_vendors-central_data-central-datax-stcd1 = 'X'.
    ls_vendors-central_data-central-datax-stcd3 = 'X'.
    ls_vendors-central_data-central-datax-brsch = 'X'.

*   Set the Company Code and GL Account
    REFRESH: lt_company[].
    CLEAR ls_company.
    ls_company-task               = 'I'.
    ls_company-data_key-bukrs     = me->a_bukrs.  "gv_ccode. "Empresa

*-CS2022000535-03.02.2023-#78407-JT-inicio
    ls_company-data-akont         = me->set_recupera_conta( EXPORTING i_bukrs = me->a_bukrs
                                                                      i_lifnr = i_lfa1-lifnr
                                                                      i_stcd1 = i_lfa1-stcd1
                                                                      i_stcd2 = i_lfa1-stcd2 ).
    ls_company-data-akont         = COND #( WHEN ls_company-data-akont IS INITIAL THEN i_lfb1-akont "gv_akont. "Cta.de reconciliaçãO
                                                                                  ELSE ls_company-data-akont ).
*-CS2022000535-03.02.2023-#78407-JT-fim

*   ls_company-data-akont         = i_lfb1-akont. "gv_akont. "Cta.de reconciliaçãO "-CS2022000535-03.02.2023-#78407-JT
    ls_company-data-zuawa         = i_lfb1-zuawa. "Chave para a ordenação por nºs atribuição
    ls_company-data-fdgrv         = i_lfb1-fdgrv. "Grupo de administração de tesouraria
    ls_company-data-zterm         = i_lfb1-zterm. "Chave de condições de pagamento
    ls_company-data-zwels         = i_lfb1-zwels. "Formas de pagamentos a considerar
    ls_company-data-qland         = i_lfb1-qland. "Código do país relativo ao imposto retido na fonte


    ls_company-datax-akont        = 'X'.
    ls_company-datax-zuawa        = 'X'.
    ls_company-datax-fdgrv        = 'X'.
    ls_company-datax-zterm        = 'X'.
    ls_company-datax-zwels        = 'X'.
    ls_company-datax-qland        = 'X'.

    IF t_lfbw IS NOT INITIAL.
      LOOP AT t_lfbw INTO DATA(ws_lfbw).
        ls_company_wtax_type_task            = 'I'.
        ls_company_wtax_type_data_key-witht  = ws_lfbw-witht.
        ls_company_wtax_type_data-wt_subjct  = ws_lfbw-wt_subjct.
        ls_company_wtax_type_data-qsrec      = ws_lfbw-qsrec.
        ls_company_wtax_type_data-wt_withcd  = ws_lfbw-wt_withcd.
        ls_company_wtax_type_datax-wt_subjct = 'X'.
        ls_company_wtax_type_datax-qsrec     = 'X'.
        ls_company_wtax_type_datax-wt_withcd = 'X'.

        ls_wtax_type_data-task               = ls_company_wtax_type_task.
        ls_wtax_type_data-data_key           = ls_company_wtax_type_data_key.
        ls_wtax_type_data-data               = ls_company_wtax_type_data.
        ls_wtax_type_data-datax              = ls_company_wtax_type_datax.
        APPEND ls_wtax_type_data            TO lt_wtax_type_data.

        ls_company-wtax_type-wtax_type[]     = lt_wtax_type_data[].
        CLEAR: ls_wtax_type_data, ws_lfbw.
      ENDLOOP.
    ENDIF.



    APPEND ls_company TO lt_company.
    ls_company_data-company = lt_company[].   "Interface externa: dados da empresa
    ls_vendors-company_data = ls_company_data.


* Inicio - FA - 30.09.2025 - BUG 190658 Erro expansão cadastro de proprietário de veiculo
    IF ws_lfm1-ekorg IS NOT INITIAL.
* Fim - FA - 30.09.2025 - BUG 190658 Erro expansão cadastro de proprietário de veiculo


*   Set the Purchasing Data
      ls_purchasing-task           = 'U'.
      ls_purchasing-data_key-ekorg = ws_lfm1-ekorg.   "Organização de compras
      ls_purchasing-data-waers     = ws_lfm1-waers.   "Moeda do pedido
      ls_purchasing-data-kalsk     = ''.              "Grupo para esquema de cálculo (fornecedor)
      ls_purchasing-data-webre     = 'X'.             "Código p/revisão de faturas baseado na entrada mercadorias
      ls_purchasing-datax-kalsk    = 'X'.
      ls_purchasing-datax-webre    = 'X'.
      ls_purchasing-datax-waers    = 'X'.


*   Set the Purchasing Data – Partner functions
      ls_purchasing-functions-functions  = lt_purch_func[]. "Interface externa: funções de parceiro
      APPEND ls_purchasing TO lt_purchasing.

* Inicio - FA - 30.09.2025 - BUG 190658 Erro expansão cadastro de proprietário de veiculo
    ENDIF.
* Fim - FA - 30.09.2025 - BUG 190658 Erro expansão cadastro de proprietário de veiculo


*   Set the Purchasing Data
    ls_purchas_data-purchasing = lt_purchasing[]. "Interface externa: organização de compras
    ls_vendors-purchasing_data = ls_purchas_data.
    APPEND ls_vendors TO lt_vendors.

* Defina os dados do fornecedor final com base nos quais eles devem ser criados
* Observe se vários fornecedores devem ser criados ... mantenha várias entradas em LT_VENDORS

    APPEND LINES OF  lt_vendors[] TO me->gs_vmds_extern-vendors.

*    lo_data->gs_vmds_extern-vendors = lt_vendors[].
  ENDMETHOD.


  METHOD prepare_dados_impostos.

    DATA: gs_succ_messages              TYPE cvis_message,
          gs_vmds_error                 TYPE vmds_ei_main,
          gs_err_messages               TYPE cvis_message,
          gs_vmds_succ                  TYPE vmds_ei_main,
          gv_lifnr                      TYPE lifnr,
          gv_bukrs                      TYPE bukrs,
          lt_contacts                   TYPE vmds_ei_contacts_t,
          ls_contacts                   TYPE vmds_ei_contacts,
          lt_vendors                    TYPE vmds_ei_extern_t,
          ls_vendors                    TYPE vmds_ei_extern,
          ls_address                    TYPE cvis_ei_address1,
          lt_company                    TYPE vmds_ei_company_t,
          ls_company                    TYPE vmds_ei_company,
          ls_company_wtax_type          TYPE vmds_ei_company-wtax_type-wtax_type, "Incrementar workearia de imposto.
          ls_company_wtax_type_task     TYPE vmd_ei_wtax_task, "Incrementar workearia de imposto.
          ls_company_wtax_type_data_key TYPE vmds_ei_wtax_key, "Incrementar workearia de imposto.
          ls_company_wtax_type_data     TYPE vmds_ei_wtax_dat, "Incrementar workearia de imposto.
          ls_company_wtax_type_datax    TYPE vmds_ei_wtax_datax, "Incrementar workearia de imposto.
          lt_wtax_type_data             TYPE vmds_ei_wtax_type_t,
          ls_wtax_type_data             TYPE vmds_ei_wtax_type,
          ls_company_data               TYPE vmds_ei_vmd_company,
          ls_purchas_data               TYPE vmds_ei_vmd_purchasing,
          lt_purchasing                 TYPE vmds_ei_purchasing_t,
          ls_purchasing                 TYPE vmds_ei_purchasing,
          lt_purch_func                 TYPE vmds_ei_functions_t,
          ls_purch_func                 TYPE vmds_ei_functions,
          ls_message                    TYPE cvis_message,
          lv_contactid                  TYPE bapicontact_01-contact,
          t_lfbw                        TYPE TABLE OF lfbw.

    FREE: lt_company[].

    SELECT *
      FROM lfbw
      INTO TABLE t_lfbw
     WHERE lifnr = i_lfa1-lifnr.

    SORT t_lfbw BY lifnr bukrs witht.

    CLEAR: ls_company,
           gs_vmds_extern.

    ls_vendors-header-object_instance-lifnr   = i_lfa1-lifnr.
    ls_vendors-header-object_task             = 'U'.      "Interface externa: código de modificação objeto
*
    ls_company-data_key-bukrs                 = me->a_bukrs.  "gv_ccode. "Empresa
    ls_company-data-qland                     = '508'.
    ls_company-datax-qland                    = 'X'.
    ls_company-task                           = 'U'.


    LOOP AT t_zfit0174 INTO DATA(w_zfit0174).
      READ TABLE t_lfbw INTO DATA(w_lfbw) WITH KEY lifnr = i_lfa1-lifnr
                                                   bukrs = me->a_bukrs
                                                   witht = w_zfit0174-witht
                                          BINARY SEARCH.
      DATA(l_subrc) = sy-subrc.

      IF     w_zfit0174-incide = abap_true  AND l_subrc <> 0.
        ls_company_wtax_type_task            = 'I'.
      ELSEIF w_zfit0174-incide = abap_false AND l_subrc  = 0.
        ls_company_wtax_type_task            = 'D'.
      ELSE.
        CONTINUE.
      ENDIF.

      ls_company_wtax_type_data_key-witht  = w_zfit0174-witht.
      ls_company_wtax_type_data-wt_subjct  = abap_true.
      ls_company_wtax_type_data-wt_withcd  = w_zfit0174-wt_withcd.
      ls_company_wtax_type_datax-wt_subjct = 'X'.
      ls_company_wtax_type_datax-wt_withcd = 'X'.

      ls_wtax_type_data-task               = ls_company_wtax_type_task.
      ls_wtax_type_data-data_key           = ls_company_wtax_type_data_key.
      ls_wtax_type_data-data               = ls_company_wtax_type_data.
      ls_wtax_type_data-datax              = ls_company_wtax_type_datax.
      APPEND ls_wtax_type_data            TO lt_wtax_type_data.

      ls_company-wtax_type-wtax_type[]     = lt_wtax_type_data[].
      CLEAR: ls_wtax_type_data.
    ENDLOOP.

    APPEND ls_company                     TO lt_company.
    ls_company_data-company                = lt_company[].   "Interface externa: dados da empresa
    ls_vendors-company_data                = ls_company_data.
    APPEND ls_vendors                     TO lt_vendors.

*-------------------------------------------------
*-- Defina os dados do fornecedor final com base nos quais eles devem ser criados
*-- Observe se vários fornecedores devem ser criados ... mantenha várias entradas em LT_VENDORS
*-------------------------------------------------
    APPEND LINES OF  lt_vendors[]         TO me->gs_vmds_extern-vendors.

  ENDMETHOD.


  METHOD SET_EMPRESA.
    clear: me->a_bukrs.
    me->a_bukrs = i_bukrs.
  ENDMETHOD.


  METHOD exect_bloq_forn.

    DATA: gs_vmds_extern TYPE vmds_ei_main,
*          gs_succ_messages TYPE cvis_message,
          gs_vmds_error  TYPE vmds_ei_main,
*          gs_err_messages  TYPE cvis_message,
          gs_vmds_succ   TYPE vmds_ei_main,
          gv_ktokk       TYPE ktokk,
          gv_ccode       TYPE bukrs,
          gv_akont       TYPE akont,
          gv_name        TYPE name1.


    DATA: lv_return TYPE i.
    CLEAR: gs_vmds_succ, gs_vmds_error.

* Declaração de dados locais
    DATA: lt_vendors      TYPE vmds_ei_extern_t,
          ls_vendors      TYPE vmds_ei_extern,
          ls_message      TYPE cvis_message,
          lt_central_data TYPE TABLE OF vmds_ei_vmd_central_data,
          ls_central_data TYPE vmds_ei_vmd_central_data.


    CLEAR gs_vmds_extern.


    SELECT SINGLE * FROM lfa1 INTO @DATA(ws_lfa1) WHERE lifnr EQ @i_lfa1-lifnr.
    IF sy-subrc EQ 0.

      ls_vendors-header-object_instance-lifnr   = i_lfa1-lifnr.
      ls_vendors-header-object_task             = 'U'.      "Interface externa: código de modificação objeto

      ls_vendors-central_data-central-data-sperr = i_lfa1-sperr.
      ls_vendors-central_data-central-data-sperm = i_lfa1-sperm.
      ls_vendors-central_data-central-data-sperq = i_lfa1-sperq.
      ls_vendors-central_data-central-data-nodel = i_lfa1-nodel.
      ls_vendors-central_data-central-data-loevm = i_lfa1-loevm.
      APPEND ls_vendors TO lt_vendors.
      CLEAR: ls_vendors.

      APPEND LINES OF  lt_vendors[] TO me->gs_vmds_extern-vendors.


      IF me->gs_vmds_extern-vendors IS INITIAL.
        EXIT.
      ENDIF.

*   Initialize all the data
      vmd_ei_api=>initialize( ).

*
      CALL METHOD vmd_ei_api=>maintain_bapi
        EXPORTING
          is_master_data           = me->gs_vmds_extern
        IMPORTING
          es_master_data_correct   = gs_vmds_succ
          es_message_correct       = gs_succ_messages
          es_master_data_defective = gs_vmds_error
          es_message_defective     = gs_err_messages.

      IF gs_err_messages-is_error IS INITIAL.
        COMMIT WORK.
*      return = gs_succ_messages.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_recupera_conta.

    FREE: e_akont.

    SELECT SINGLE akont
      INTO e_akont
      FROM lfb1
     WHERE lifnr = i_lifnr
       AND bukrs = i_bukrs.

    IF sy-subrc = 0 AND e_akont IS NOT INITIAL.
      EXIT.
    ENDIF.

*---------------------
* parametro
*---------------------
    SELECT *
      INTO @DATA(w_0190)
      FROM zfit0190
        UP TO 1 ROWS
     WHERE bukrs     = @i_bukrs
       AND cpf       = @i_stcd2
       AND cnpj_raiz = @abap_off.
    ENDSELECT.

    IF sy-subrc = 0.
      e_akont = w_0190-akont.
    ELSE.
      SELECT *
        INTO w_0190
        FROM zfit0190
          UP TO 1 ROWS
       WHERE bukrs     = i_bukrs
         AND cpf       = abap_off
         AND cnpj_raiz = i_stcd1(8).
      ENDSELECT.

      IF sy-subrc = 0.
        e_akont = w_0190-akont.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
