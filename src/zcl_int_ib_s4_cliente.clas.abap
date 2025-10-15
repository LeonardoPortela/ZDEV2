CLASS zcl_int_ib_s4_cliente DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_erros,
        mensagem TYPE char100,
      END OF ty_erros .


    DATA:
      tl_erro TYPE  TABLE OF ty_erros .

    TYPES:
      BEGIN OF ty_erro,
        idorigem TYPE  char20,
        erros    LIKE tl_erro,
      END OF ty_erro .


    DATA:
      BEGIN OF zde_data_response,
        erros TYPE TABLE OF ty_erro,
      END OF zde_data_response .

    METHODS constructor .
    METHODS mt_fill_part_cust_header
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !im_create      TYPE abap_bool
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_data
      IMPORTING
        !is_kna1    TYPE kna1
      CHANGING
        !cs_bp_data TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_address
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !is_kna1        TYPE kna1
        !is_addr1_data  TYPE addr1_data OPTIONAL
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_contact
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !is_kna1        TYPE kna1
        !is_sza1_d0100  TYPE sza1_d0100
        !is_knvk        TYPE knvk
        !it_adsmtp      TYPE bbpt_er_adsmtp
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_tax
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !it_knvi        TYPE cvis_knvi_t
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_company
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !it_knb1        TYPE cvis_knb1_t
        !it_knbw        TYPE cvis_knbw_t
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_sales
      IMPORTING
        !im_object_task TYPE bus_ei_object_task
        !it_knvv        TYPE cvis_knvv_t
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_fill_part_cust_bankdetail
      IMPORTING
        !im_object_task TYPE cvi_ei_bank_task
        !it_knbk        TYPE knbk_t
      CHANGING
        !cs_bp_data     TYPE cvis_ei_extern .
    METHODS mt_move_data
      IMPORTING
        !is_from  TYPE any
      CHANGING
        !cs_data  TYPE any
        !cs_datax TYPE any .
    METHODS mt_consist_tax
      IMPORTING
        !im_kunnr      TYPE kna1-kunnr
        !im_stcd1      TYPE kna1-stcd1
        !im_stcd2      TYPE kna1-stcd2
        !im_stcd3      TYPE kna1-stcd3
        !im_ktokd      TYPE kna1-ktokd
      CHANGING
        !cs_duplic_flg TYPE char1
        !cs_kunnret    TYPE kna1-kunnr .
protected section.
private section.

  data AT_ID_INTERFACE type ZDE_ID_INTERFACE value '156' ##NO_TEXT.

  methods MT_FILL_PART_HEADER_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_PARTNER type BU_PARTNER
      !IM_GUID type BU_PARTNER_GUID_BAPI
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_COMMON_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_BU_TYPE type BU_TYPE
      !IS_KNA1 type KNA1
      !IS_ADDR1_DATA type ADDR1_DATA
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_ADDRESS_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_ADDR1_DATA type ADDR1_DATA
      !IS_SZA1_D0100 type SZA1_D0100
      !IS_KNA1 type KNA1
      !IT_ADSMTP type BBPT_ER_ADSMTP
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_TAX_CUST
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IS_KNA1 type KNA1
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_FILL_PART_CENT_ROLES_SUP
    importing
      !IM_OBJECT_TASK type BUS_EI_OBJECT_TASK
      !IM_ROLE type BU_ROLE
    changing
      !CS_BP_DATA type CVIS_EI_EXTERN .
  methods MT_GET_BP_GUID
    importing
      !IM_PARTNER type BU_PARTNER
    changing
      !CM_PARTNER_GUID type BU_PARTNER_GUID .
ENDCLASS.



CLASS ZCL_INT_IB_S4_CLIENTE IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface      = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound. "Out
    me->zif_integracao_inject~at_tp_canal          = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia      = zif_integracao=>at_tp_sincronia_assincrona.
    me->zif_integracao_inject~at_autentica_opus    = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_autentica_api_ad  = zif_integracao=>at_id_interface_aut_api_ad_nao.
    me->zif_integracao_inject~at_send_autenticao   = zif_integracao=>at_id_interface_aut_send_sim.
    me->zif_integracao_inject~at_autentica_module  = 'CADASTRO_CLIENTE'.

  ENDMETHOD.


  METHOD mt_consist_tax.

    DATA: lva_stcd2 TYPE kna1-stcd2.

    IF ( 'ZCNF ZCFU ZCFF' CS im_ktokd ).
*  Não deixar duplicar cpf
      CHECK NOT im_stcd2 IS INITIAL.

      SELECT SINGLE kunnr stcd2 INTO (cs_kunnret , lva_stcd2)
         FROM kna1 "UP TO 1 ROWS
         WHERE stcd2 = im_stcd2 AND
               "kunnr <> im_kunnr AND
               ktokd =  im_ktokd.
      "ENDSELECT.

      IF ( sy-subrc EQ 0 ).
        cs_duplic_flg = 'X'.
      ENDIF.


    ELSEIF ( 'ZCNJ ZCPJ ZCIC ZCFJ' CS im_ktokd ).
*  Não deixar duplicar CNPJ
      DATA: lva_stcd1 TYPE kna1-stcd1.

      CHECK NOT im_stcd1 IS INITIAL.

      SELECT SINGLE kunnr stcd1 INTO (cs_kunnret, lva_stcd1)
         FROM kna1 "UP TO 1 ROWS
         WHERE stcd1 = im_stcd1 AND
               "kunnr <> im_kunnr AND
               ktokd =  im_ktokd. .
      "ENDSELECT.

      IF ( sy-subrc EQ 0 ).
        cs_duplic_flg = 'X'.
      ENDIF.

    ELSEIF ( im_ktokd EQ 'ZCPF'  ).
*  Não deixar duplicar CPF + Inscrição estadual juntos

      CHECK NOT im_stcd2 IS INITIAL.

      "Seleciona apenas o que não está bloqueado para todas as empresas (SPERR) e o que não está marcado para eliminação (LOEVM).
      SELECT SINGLE kunnr stcd2 INTO (cs_kunnret, lva_stcd2)
         FROM kna1 "UP TO 1 ROWS
         WHERE stcd2 =  im_stcd2    AND
               stcd3 =  im_stcd3    AND
               "kunnr <> im_kunnr    AND
               ktokd =  im_ktokd    AND
               sperr =  abap_false  AND
               loevm =  abap_false.
      "ENDSELECT.
      IF ( sy-subrc EQ 0 ).
        cs_duplic_flg = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mt_fill_part_cent_address_cust.

    DATA:
      lt_addresses_all TYPE STANDARD TABLE OF bapibus1006_addresses,
      lt_return	       TYPE STANDARD TABLE OF bapiret2,
      l_insr           TYPE c.

    IF is_addr1_data IS NOT INITIAL.

      l_insr = 'X'.

      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<fs_address>).
      <fs_address>-data_key-guid = ''.
*      <fs_address>-task          = im_object_task.

      IF im_object_task EQ 'U'.

        <fs_address>-task          = 'M'.

        IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

          CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
            EXPORTING
              businesspartner = cs_bp_data-partner-header-object_instance-bpartner
            TABLES
              addresses_all   = lt_addresses_all
              return          = lt_return.

          READ TABLE lt_addresses_all ASSIGNING FIELD-SYMBOL(<fs_addr>)
            WITH KEY standardaddress = 'X'.

          IF sy-subrc EQ 0.

            <fs_address>-data_key-guid = <fs_addr>-addressguid.

          ENDIF.

          CLEAR: cs_bp_data-ensure_create-create_customer.

        ENDIF.

      ELSE.

        <fs_address>-task          = im_object_task.

      ENDIF.

*--> Precisa de ajuste para o form funcionar aqui.
*    perform ZF_MOVE_DATA
*      using iS_ADDR1_DATA
*      changing <FS_ADDRESS>-DATA-POSTAL-DATA
*               <FS_ADDRESS>-DATA-POSTAL-DATAX.

      MOVE-CORRESPONDING is_addr1_data TO <fs_address>-data-postal-data.
      IF NOT is_addr1_data-street IS INITIAL.
        <fs_address>-data-postal-datax-street = 'X'.
      ENDIF.
      IF NOT is_addr1_data-country IS INITIAL.
        <fs_address>-data-postal-datax-country = 'X'.
      ENDIF.
      IF NOT is_addr1_data-region IS INITIAL.
        <fs_address>-data-postal-datax-region = 'X'.
      ENDIF.
      IF NOT is_addr1_data-transpzone IS INITIAL.
        <fs_address>-data-postal-datax-transpzone = 'X'.
      ENDIF.
      IF NOT is_addr1_data-taxjurcode IS INITIAL.
        <fs_address>-data-postal-datax-taxjurcode = 'X'.
      ENDIF.
*<-- Precisa de ajuste para o form funcionar aqui.

      <fs_address>-task                         = im_object_task.
      <fs_address>-data-postal-data-city        = is_addr1_data-city1.
      <fs_address>-data-postal-datax-city       = 'X'.
      <fs_address>-data-postal-data-district    = is_addr1_data-city2.
      <fs_address>-data-postal-datax-district   = 'X'.
      <fs_address>-data-postal-data-langu       = is_addr1_data-langu.
      <fs_address>-data-postal-datax-langu      = 'X'.
      <fs_address>-data-postal-data-house_no    = is_addr1_data-house_num1.
      <fs_address>-data-postal-datax-house_no   = 'X'.
      <fs_address>-data-postal-data-house_no2   = is_addr1_data-house_num2.
      <fs_address>-data-postal-datax-house_no2  = 'X'.
      <fs_address>-data-postal-data-postl_cod1  = is_addr1_data-post_code1.
      <fs_address>-data-postal-datax-postl_cod1 = 'X'.
      <fs_address>-data-postal-data-postl_cod2  = is_addr1_data-post_code2.
      <fs_address>-data-postal-datax-postl_cod2 = 'X'.
      <fs_address>-data-postal-data-langu       = sy-langu.
      <fs_address>-data-postal-datax-langu      = 'X'.

      APPEND INITIAL LINE TO <fs_address>-data-addr_usage-addr_usages ASSIGNING FIELD-SYMBOL(<fs_usage>).
      <fs_usage>-currently_valid = abap_true.
      <fs_usage>-task = im_object_task.
      <fs_usage>-data_key-addresstype = 'XXDEFAULT'.

      IF is_sza1_d0100-tel_number IS NOT INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phone>)." Comunicação
        <fs_phone>-contact-task            = im_object_task.
        <fs_phone>-contact-data-tel_no     = is_sza1_d0100-tel_number.
        <fs_phone>-contact-datax-tel_no    = 'X'.
        <fs_phone>-contact-data-telephone  = is_sza1_d0100-tel_number.
        <fs_phone>-contact-datax-telephone = 'X'.
      ENDIF.
      IF NOT is_sza1_d0100-mob_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-phone-phone ASSIGNING <fs_phone>." Comunicação
        <fs_phone>-contact-task            = im_object_task.
        <fs_phone>-contact-data-tel_no     = is_sza1_d0100-mob_number.
        <fs_phone>-contact-datax-tel_no    = 'X'.
        <fs_phone>-contact-data-telephone  = is_sza1_d0100-mob_number.
        <fs_phone>-contact-datax-telephone = 'X'.
        <fs_phone>-contact-data-r_3_user   = '2'.
      ENDIF.
      IF NOT is_sza1_d0100-fax_number IS INITIAL.
        APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_cfax>)." Comunicação
        <fs_cfax>-contact-task         = im_object_task.
        <fs_cfax>-contact-data-fax_no  = is_sza1_d0100-fax_number.
        <fs_cfax>-contact-datax-fax_no = 'X'.
        <fs_cfax>-contact-data-fax     = is_sza1_d0100-fax_number.
        <fs_cfax>-contact-datax-fax    = 'X'.
      ENDIF.
      IF NOT is_sza1_d0100-smtp_addr IS INITIAL.
        DATA: lv_count TYPE i.
        ADD 1 TO lv_count.
        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_cemail>)." Comunicação
        <fs_cemail>-contact-task            = im_object_task.
        <fs_cemail>-contact-data-e_mail     = is_sza1_d0100-smtp_addr.
        <fs_cemail>-contact-datax-e_mail    = 'X'.
        <fs_cemail>-contact-data-std_no     = abap_true.
        <fs_cemail>-contact-datax-std_no    = 'X'.
        <fs_cemail>-contact-data-consnumber = lv_count.
        LOOP AT it_adsmtp ASSIGNING FIELD-SYMBOL(<fs_adsmtp>).
          ADD 1 TO lv_count.
          APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING <fs_cemail>.
          <fs_cemail>-contact-data-consnumber = lv_count.
          <fs_cemail>-contact-task            = im_object_task.
          <fs_cemail>-contact-data-e_mail     = <fs_adsmtp>-smtp_addr.
          <fs_cemail>-contact-datax-e_mail    = 'X'.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF is_kna1-telfx IS NOT INITIAL.

      IF l_insr IS INITIAL AND <fs_address> IS NOT ASSIGNED.

        l_insr = 'X'.

        APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING <fs_address>.
        <fs_address>-data_key-guid = ''.

        IF im_object_task EQ 'U'.

          <fs_address>-task          = 'M'.

          IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

            CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
              TABLES
                addresses_all   = lt_addresses_all
                return          = lt_return.

            READ TABLE lt_addresses_all ASSIGNING <fs_addr>
              WITH KEY standardaddress = 'X'.

            IF sy-subrc EQ 0.
              <fs_address>-data_key-guid = <fs_addr>-addressguid.
            ENDIF.
            CLEAR: cs_bp_data-ensure_create-create_customer.
          ENDIF.
        ELSE.
          <fs_address>-task          = im_object_task.
        ENDIF.
      ENDIF.

      IF <fs_address> IS ASSIGNED.
        APPEND INITIAL LINE TO <fs_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fs_fax>).
        <fs_fax>-contact-task      = im_object_task.
        <fs_fax>-contact-data-fax  = is_kna1-telfx.
        <fs_fax>-contact-datax-fax = 'X'.
      ENDIF.
    ENDIF.

    IF is_sza1_d0100-smtp_addr IS NOT INITIAL.

      IF l_insr IS INITIAL AND <fs_address> IS NOT ASSIGNED.
        l_insr = 'X'.

        APPEND INITIAL LINE TO cs_bp_data-partner-central_data-address-addresses ASSIGNING <fs_address>.
        <fs_address>-data_key-guid = ''.

        IF im_object_task EQ 'U'.
          <fs_address>-task          = 'M'.
          IF cs_bp_data-partner-header-object_instance-bpartner IS NOT INITIAL.

            CALL FUNCTION 'BAPI_BUPA_ADDRESSES_GET'
              EXPORTING
                businesspartner = cs_bp_data-partner-header-object_instance-bpartner
              TABLES
                addresses_all   = lt_addresses_all
                return          = lt_return.

            READ TABLE lt_addresses_all ASSIGNING <fs_addr>
              WITH KEY standardaddress = 'X'.

            IF sy-subrc EQ 0.
              <fs_address>-data_key-guid = <fs_addr>-addressguid.
            ENDIF.
            CLEAR: cs_bp_data-ensure_create-create_customer.
          ENDIF.
        ELSE.
          <fs_address>-task          = im_object_task.
        ENDIF.
      ENDIF.
      IF <fs_address> IS ASSIGNED.
        APPEND INITIAL LINE TO <fs_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
        <fs_smtp>-contact-task         = im_object_task.
        <fs_smtp>-contact-data-e_mail  = is_sza1_d0100-smtp_addr.
        <fs_smtp>-contact-datax-e_mail = 'X'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD mt_fill_part_cent_common_cust.

    cs_bp_data-partner-central_data-common-data-bp_control-category = im_bu_type.
    IF NOT is_addr1_data-sort1 IS INITIAL.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_addr1_data-sort1.
    ELSE.
      cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm1 = is_kna1-sortl.
    ENDIF.

    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm1     = abap_true.
    cs_bp_data-partner-central_data-common-data-bp_centraldata-searchterm2      = is_addr1_data-sort2.
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-searchterm2     = abap_true.
    cs_bp_data-partner-central_data-common-data-bp_centraldata-partnerlanguage  = sy-langu.
    cs_bp_data-partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.

    CASE im_bu_type.
      WHEN '1'. "Person
        IF is_addr1_data-title = '0002'.
          cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = '0002'. "Masculino
        ELSE.
          IF is_addr1_data-title = '0001'.
            cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = '0001'. "Feminino
          ENDIF.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_person-firstname           = is_kna1-name1.

        IF cs_bp_data-partner-central_data-common-data-bp_person-firstname IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_person-firstname          = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_person-lastname            = is_kna1-name2.

        IF cs_bp_data-partner-central_data-common-data-bp_person-lastname IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_person-lastname           = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_person-nationality         = is_kna1-land1.
        cs_bp_data-partner-central_data-common-datax-bp_person-nationality        = 'X'.
        cs_bp_data-partner-central_data-common-data-bp_person-correspondlanguage  = sy-langu.
        cs_bp_data-partner-central_data-common-datax-bp_person-correspondlanguage = 'X'.

        IF cs_bp_data-partner-central_data-common-data-bp_person-firstname IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-firstname  = is_addr1_data-name1.
          cs_bp_data-partner-central_data-common-datax-bp_person-firstname = 'X'.
        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_person-lastname IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_person-lastname  = is_addr1_data-name2.
          cs_bp_data-partner-central_data-common-datax-bp_person-lastname = 'X'.
        ENDIF.

      WHEN '2'. "Organization
        cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_kna1-name1.
        cs_bp_data-partner-central_data-common-data-bp_centraldata-title_key      = '0003'. "Empresa

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name1 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name1 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_kna1-name2.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name2 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name2 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name3  = is_kna1-name3.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name3 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name3 = 'X'.
        ENDIF.

        cs_bp_data-partner-central_data-common-data-bp_organization-name4  = is_kna1-name4.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name4 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name4 = 'X'.
        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name1 IS INITIAL AND
           is_addr1_data-name1 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_organization-name1  = is_addr1_data-name1.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name1 = 'X'.
        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name2 IS INITIAL AND
           is_addr1_data-name2 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_organization-name2  = is_addr1_data-name2.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name2 = 'X'.
        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name3 IS INITIAL AND
           is_addr1_data-name3 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_organization-name3 = is_addr1_data-name3.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name3 = 'X'.
        ENDIF.

        IF cs_bp_data-partner-central_data-common-data-bp_organization-name4 IS INITIAL AND
           is_addr1_data-name4 IS NOT INITIAL.
          cs_bp_data-partner-central_data-common-data-bp_organization-name4 = is_addr1_data-name4.
          cs_bp_data-partner-central_data-common-datax-bp_organization-name4 = 'X'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD mt_fill_part_cent_roles_sup.

    CONSTANTS c_valid_to TYPE d VALUE '99991231'.

    cs_bp_data-partner-central_data-role-current_state = abap_false. "-> Se deixar TRUE, a classe apaga as outras roles existentes

    APPEND INITIAL LINE TO cs_bp_data-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<fs_role>).
    <fs_role>-task              = im_object_task.
    <fs_role>-data_key          = im_role.
    <fs_role>-currently_valid   = abap_true.
*  <FS_ROLE>-DATA-ROLECATEGORY = IM_ROLE.
    <fs_role>-data-valid_from   = sy-datum.
    <fs_role>-data-valid_to     = c_valid_to.
    <fs_role>-datax-valid_from  = abap_true.
    <fs_role>-datax-valid_to    = abap_true.
    UNASSIGN <fs_role>.

  ENDMETHOD.


  METHOD mt_fill_part_cent_tax_cust.

    IF NOT is_kna1-stcd1 IS INITIAL
    OR NOT is_kna1-stcd2 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<fs_taxnum>).
      <fs_taxnum>-task = im_object_task.
      IF is_kna1-stcd1 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_kna1-stcd1.
        <fs_taxnum>-data_key-taxtype = 'BR1'.
      ELSEIF is_kna1-stcd2 IS NOT INITIAL.
        <fs_taxnum>-data_key-taxnumber = is_kna1-stcd2.
        <fs_taxnum>-data_key-taxtype   = 'BR2'.
        cs_bp_data-partner-central_data-taxnumber-common-data-nat_person = 'X'.
      ENDIF.
    ENDIF.

    IF NOT is_kna1-stcd3 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
      <fs_taxnum>-task = im_object_task.
      <fs_taxnum>-data_key-taxnumber = is_kna1-stcd3.
      <fs_taxnum>-data_key-taxtype = 'BR3'.
    ENDIF.

    IF NOT is_kna1-stcd4 IS INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnum>.
      <fs_taxnum>-task = im_object_task.
      <fs_taxnum>-data_key-taxnumber = is_kna1-stcd4.
      <fs_taxnum>-data_key-taxtype = 'BR4'.
    ENDIF.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_ADDRESS.

*  CS_BP_DATA-customer-central_data-central-data-katr1  = IS_ADDR1_DATA-extension2.
*  CS_BP_DATA-customer-central_data-central-datax-katr1 = abap_true.
*  CS_BP_DATA-customer-central_data-central-data-katr6  = IS_ADDR1_DATA-extension1.
*  CS_BP_DATA-customer-central_data-central-datax-katr6 = abap_true.

    cs_bp_data-customer-central_data-central-data-katr1  = is_addr1_data-extension2.
    cs_bp_data-customer-central_data-central-datax-katr1 = abap_true.
    cs_bp_data-customer-central_data-central-data-katr6  = is_addr1_data-extension1.
    cs_bp_data-customer-central_data-central-datax-katr6 = abap_true.

    cs_bp_data-customer-central_data-address-task = im_object_task.
    cs_bp_data-customer-central_data-address-postal-data-name = is_kna1-name1.
    cs_bp_data-customer-central_data-address-postal-datax-name = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_2 = is_kna1-name2.
    cs_bp_data-customer-central_data-address-postal-datax-name_2 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_3 = is_kna1-name3.
    cs_bp_data-customer-central_data-address-postal-datax-name_3 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-name_4 = is_kna1-name4.
    cs_bp_data-customer-central_data-address-postal-datax-name_4 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-country = is_kna1-land1.
    cs_bp_data-customer-central_data-address-postal-datax-country = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-location = is_kna1-locco.
    cs_bp_data-customer-central_data-address-postal-datax-location = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-city = is_addr1_data-city1.
    cs_bp_data-customer-central_data-address-postal-datax-city = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-district = is_addr1_data-city2.
    cs_bp_data-customer-central_data-address-postal-datax-district = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-po_box = is_addr1_data-po_box.
    cs_bp_data-customer-central_data-address-postal-datax-po_box = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-postl_cod1 = is_addr1_data-post_code1.
    cs_bp_data-customer-central_data-address-postal-datax-postl_cod1 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-postl_cod2 = is_addr1_data-post_code2.
    cs_bp_data-customer-central_data-address-postal-datax-postl_cod2 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-region = is_kna1-regio.
    cs_bp_data-customer-central_data-address-postal-datax-region = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-sort1 = is_kna1-sortl.
    cs_bp_data-customer-central_data-address-postal-datax-sort1 = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-street = is_addr1_data-street.
    cs_bp_data-customer-central_data-address-postal-datax-street = abap_true.
    cs_bp_data-customer-central_data-address-postal-data-taxjurcode = is_kna1-txjcd.
    cs_bp_data-customer-central_data-address-postal-datax-taxjurcode = abap_true.
    MOVE-CORRESPONDING is_addr1_data TO cs_bp_data-customer-central_data-address-postal-data.
    cs_bp_data-customer-central_data-address-postal-data-langu = sy-langu.
    cs_bp_data-customer-central_data-address-postal-datax-langu = abap_true.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_BANKDETAIL.

    LOOP AT it_knbk ASSIGNING FIELD-SYMBOL(<fs_knbk>).

      APPEND VALUE #(
        task = 'M'
        data-bank_ctry     = 'BR'
        data-bank_key      = <fs_knbk>-bankl
        data-bank_acct     = <fs_knbk>-bankn
        data-ctrl_key      = <fs_knbk>-bkont
        data-accountholder = <fs_knbk>-koinh

        datax-bank_ctry     = abap_true
        datax-bank_key      = abap_true
        datax-bank_acct     = abap_true
        datax-ctrl_key      = abap_true
        datax-accountholder = abap_true

      ) TO cs_bp_data-partner-central_data-bankdetail-bankdetails.

    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_COMPANY.

    TYPES:
      BEGIN OF ty_knb1,
        kunnr TYPE knb1-kunnr,
        bukrs TYPE knb1-bukrs,
      END OF ty_knb1.

    DATA:
      lt_knb1   TYPE STANDARD TABLE OF ty_knb1,
      lt_knb1_p TYPE cvis_knb1_t,
      l_task    TYPE cmd_ei_company_task.

    SELECT kunnr bukrs
      INTO TABLE lt_knb1
      FROM knb1
      FOR ALL ENTRIES IN it_knb1
      WHERE kunnr EQ it_knb1-kunnr
        AND bukrs EQ it_knb1-bukrs.

    IF sy-subrc EQ 0.

      SORT lt_knb1 BY kunnr bukrs ASCENDING.

    ENDIF.

    lt_knb1_p[] = it_knb1[].

    SORT lt_knb1_p BY kunnr bukrs ASCENDING.

    LOOP AT lt_knb1_p ASSIGNING FIELD-SYMBOL(<fs_knb1>).

      l_task = im_object_task.

      READ TABLE lt_knb1 TRANSPORTING NO FIELDS
        WITH KEY kunnr = <fs_knb1>-kunnr
                 bukrs = <fs_knb1>-bukrs
                 BINARY SEARCH.

      IF sy-subrc NE 0.
        l_task = 'I'.
      ENDIF.

      APPEND INITIAL LINE TO cs_bp_data-customer-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company>).
      <fs_company>-task     = l_task.
      <fs_company>-data_key = <fs_knb1>-bukrs.

      me->mt_move_data(
           EXPORTING
             is_from  = <fs_knb1>
           CHANGING
             cs_data  = <fs_company>-data
             cs_datax = <fs_company>-datax
         ).

      LOOP AT it_knbw ASSIGNING FIELD-SYMBOL(<fs_knbw>)
                      WHERE bukrs = <fs_knb1>-bukrs.
        APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING FIELD-SYMBOL(<fs_wtax_type>).
        <fs_wtax_type>-data_key-witht = <fs_knbw>-witht.
        <fs_wtax_type>-task = l_task.

        me->mt_move_data(
     EXPORTING
       is_from  = <fs_knbw>
     CHANGING
       cs_data  = <fs_wtax_type>-data
       cs_datax = <fs_wtax_type>-datax
   ).

        UNASSIGN <fs_wtax_type>.
      ENDLOOP.
      IF sy-subrc <> 0.
        LOOP AT it_knbw ASSIGNING <fs_knbw>.
          APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type ASSIGNING <fs_wtax_type>.
          <fs_wtax_type>-data_key-witht = <fs_knbw>-witht.
          <fs_wtax_type>-task = l_task.

          me->mt_move_data(
            EXPORTING
              is_from  = <fs_knbw>
            CHANGING
              cs_data  = <fs_wtax_type>-data
              cs_datax = <fs_wtax_type>-datax
          ).

          UNASSIGN <fs_wtax_type>.
        ENDLOOP.
      ENDIF.

      UNASSIGN <fs_company>.
    ENDLOOP.

  ENDMETHOD.


  METHOD mt_fill_part_cust_contact.

    IF is_kna1-telf1 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone
      ASSIGNING FIELD-SYMBOL(<fs_phone>).
      <fs_phone>-contact-data-telephone   = is_kna1-telf1.
      <fs_phone>-contact-datax-telephone  = abap_true.
      <fs_phone>-contact-data-tel_no      = is_kna1-telf1.
      <fs_phone>-contact-datax-tel_no     = abap_true.
    ELSEIF is_sza1_d0100-tel_number IS NOT INITIAL.

      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone ASSIGNING <fs_phone>.
      <fs_phone>-contact-task             = im_object_task.
      <fs_phone>-contact-data-tel_no      = is_sza1_d0100-tel_number.
      <fs_phone>-contact-datax-tel_no     = abap_true.
      <fs_phone>-contact-data-telephone   = is_sza1_d0100-tel_number.
      <fs_phone>-contact-datax-telephone  = abap_true.

      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_smtp>).
      <fs_smtp>-contact-task           = im_object_task.
      <fs_smtp>-contact-data-e_mail    = is_sza1_d0100-smtp_addr.
      <fs_smtp>-contact-datax-e_mail   = abap_true.

      LOOP AT it_adsmtp INTO DATA(is_adsmtp).
        APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-smtp-smtp ASSIGNING <fs_smtp>.
        <fs_smtp>-contact-task           = im_object_task.
        <fs_smtp>-contact-data-e_mail    = is_adsmtp-smtp_addr.
        <fs_smtp>-contact-datax-e_mail   = abap_true.
      ENDLOOP.

      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-contact-contacts ASSIGNING FIELD-SYMBOL(<fs_contact>).
      <fs_contact>-address_type_1-task    = im_object_task.
      APPEND INITIAL LINE TO <fs_contact>-address_type_1-communication-phone-phone ASSIGNING FIELD-SYMBOL(<fs_phones>).
      <fs_phones>-contact-data-telephone  = is_sza1_d0100-tel_number.
      <fs_phones>-contact-datax-telephone = abap_true.
      <fs_phones>-contact-data-tel_no     = is_sza1_d0100-tel_number.
      <fs_phones>-contact-datax-tel_no    = abap_true.

    ENDIF.
    IF is_kna1-telf2 IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-address-communication-phone-phone
      ASSIGNING <fs_phone>.
      <fs_phone>-contact-data-telephone   = is_kna1-telf2.
      <fs_phone>-contact-datax-telephone  = abap_true.
      <fs_phone>-contact-data-tel_no      = is_kna1-telf2.
      <fs_phone>-contact-datax-tel_no     = abap_true.
    ENDIF.

    IF is_knvk IS NOT INITIAL.
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-contact-contacts ASSIGNING <fs_contact>.

      <fs_contact>-task = im_object_task.
      MOVE-CORRESPONDING is_knvk TO <fs_contact>-data.
    ENDIF.

    cs_bp_data-partner-central_data-common-data-bp_person-birthdate = is_knvk-gbdat.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_DATA.

    me->mt_move_data(
      EXPORTING
        is_from  = is_kna1
      CHANGING
        cs_data  = cs_bp_data-customer-central_data-central-data
        cs_datax = cs_bp_data-customer-central_data-central-datax
    ).


    IF cs_bp_data-customer-central_data-central-data-ktokd IS NOT INITIAL.
      "ATENÇÃO!!!
      "Existe uma regra que impede que esse dado seja definido sem ser por customização.
      "Ver classe CVI_MAPPER->MAP_BPS_TO_VENDORS e procurar por "flexible handling of account groups"
      SELECT SINGLE bu_group
        FROM tbd001
        INTO cs_bp_data-partner-central_data-common-data-bp_control-grouping
      WHERE ktokd = cs_bp_data-customer-central_data-central-data-ktokd.

    ENDIF.
  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_HEADER.
    cs_bp_data-customer-header-object_task   = im_object_task.
    cs_bp_data-ensure_create-create_customer = im_create.
  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_SALES.

    LOOP AT it_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv>).
      APPEND INITIAL LINE TO cs_bp_data-customer-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
      <fs_sales>-task = im_object_task.
      <fs_sales>-data_key-vkorg = <fs_knvv>-vkorg.
      <fs_sales>-data_key-vtweg = <fs_knvv>-vtweg.
      <fs_sales>-data_key-spart = <fs_knvv>-spart.

      me->mt_move_data(
        EXPORTING
          is_from  = <fs_knvv>
        CHANGING
          cs_data  = <fs_sales>-data
          cs_datax = <fs_sales>-datax
      ).

      UNASSIGN <fs_sales>.
    ENDLOOP.

  ENDMETHOD.


  METHOD MT_FILL_PART_CUST_TAX.

    LOOP AT it_knvi ASSIGNING FIELD-SYMBOL(<fs_knvi>).
      APPEND INITIAL LINE TO cs_bp_data-customer-central_data-tax_ind-tax_ind ASSIGNING FIELD-SYMBOL(<fs_tax>).
      <fs_tax>-task = im_object_task.
      <fs_tax>-data_key-aland = <fs_knvi>-aland.
      <fs_tax>-data_key-tatyp = <fs_knvi>-tatyp.

      me->mt_move_data(
        EXPORTING
          is_from  = <fs_knvi>
        CHANGING
          cs_data  = <fs_tax>-data
          cs_datax = <fs_tax>-datax
      ).

      UNASSIGN <fs_tax>.
    ENDLOOP.

  ENDMETHOD.


  METHOD mt_fill_part_header_sup.

    CASE im_object_task.
      WHEN 'I'.
        cs_bp_data-partner-header-object_task                  = im_object_task.
        cs_bp_data-partner-header-object_instance-bpartnerguid = cl_system_uuid=>create_uuid_x16_static( ).

      WHEN 'U' OR 'M'.
        DATA: lv_guid TYPE bu_partner_guid.

        me->mt_get_bp_guid(
          EXPORTING
            im_partner      = im_partner              " Nº parceiro de negócios
          CHANGING
            cm_partner_guid = lv_guid                 " GUID de um parceiro de negócios
        ).

        cs_bp_data-partner-header-object_task                  = im_object_task.
        cs_bp_data-partner-header-object_instance-bpartner     = im_partner.
        cs_bp_data-partner-header-object_instance-bpartnerguid = lv_guid.
    ENDCASE.
  ENDMETHOD.


  METHOD mt_get_bp_guid.

    CLEAR cm_partner_guid.
    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner      = im_partner
      IMPORTING
        ev_partner_guid = cm_partner_guid.

  ENDMETHOD.


  METHOD MT_MOVE_DATA.

    DATA lo_structure TYPE REF TO cl_abap_structdescr.
    DATA lo_exception TYPE REF TO cx_root.
    DATA lt_fields    TYPE abap_component_tab.

    FIELD-SYMBOLS <fs_f_from> TYPE any.
    FIELD-SYMBOLS <fs_f_data> TYPE any.
    FIELD-SYMBOLS <fs_f_datax> TYPE any.

    lo_structure ?= cl_abap_typedescr=>describe_by_data( cs_data ).
    lt_fields = lo_structure->get_components( ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_fields>).

      TRY.
          UNASSIGN <fs_f_from>.
          ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE is_from TO <fs_f_from>.
          CHECK sy-subrc = 0.
*          CHECK <fs_f_from> IS NOT INITIAL.
          IF <fs_f_from> IS NOT INITIAL OR
             ( <fs_f_from> IS INITIAL AND <fs_fields>-name EQ 'ZAHLS' ). " Considering Unlocking

            UNASSIGN <fs_f_data>.
            UNASSIGN <fs_f_datax>.
            ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE cs_data TO <fs_f_data>.
            CHECK sy-subrc = 0.
            ASSIGN COMPONENT <fs_fields>-name OF STRUCTURE cs_datax TO <fs_f_datax>.
            CHECK sy-subrc = 0.
            <fs_f_data> = <fs_f_from>.
            <fs_f_datax> = abap_true.

          ENDIF.

        CATCH cx_root INTO lo_exception.
          CONTINUE.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao,
          lva_msg_erro  TYPE string.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->free(
      ).

    me->zif_integracao_inbound~validar_dados_inbound( EXPORTING i_data_inbound = me->zif_integracao_inject~at_info_request_http-ds_body
                                                      IMPORTING e_status_code  = DATA(_status_code)
                                                      RECEIVING  r_msg_erro = lva_msg_erro
                                                                                  ).
    IF lva_msg_erro IS NOT INITIAL.

      IF _status_code IS INITIAL .
        _status_code = '400'. "Bad Request
      ENDIF.

      me->zif_integracao_inbound~at_zintegracao_log-nm_code = _status_code.

      e_msg = lva_msg_erro.

*      e_msg = '{  "error": "'        && lva_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
*              '   "status_code" : "' && _status_code  && '" '  && cl_abap_char_utilities=>newline &&
*                 '}'.

      UPDATE zintegracao SET ck_processado   = abap_true
                             ck_integrado    = abap_true
                             ds_data_retorno =  e_msg
                       WHERE id_integracao = me->zif_integracao_inbound~at_id_integracao.
      COMMIT WORK.

    ELSE.
      me->zif_integracao_inbound~at_zintegracao_log-nm_code = '200'.
      e_msg = '{"SUCESSO":"OK"}'.
    ENDIF.

    CLEAR: lc_integracao.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.
    me->zif_integracao_inject~at_tp_integracao     = zif_integracao=>at_tp_integracao_inbound.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INBOUND~VALIDAR_DADOS_INBOUND.

    DATA: w_data_inbound   TYPE zsdt0325_inb_cliente,
          lit_data_inbound TYPE zsdt0325_inbcli_t,
          w_tx             TYPE zsdt0327tx,
          lva_kna1         TYPE kna1,
          lva_tabix        TYPE sy-tabix,
          lva_duplic_flg   TYPE c,
          lva_kunnret      TYPE kna1-kunnr,
          lva_erro         TYPE string,
          lva_type         TYPE dd01v-datatype,
          l_erro(1).

    DATA: lwa_data_response_erro LIKE zde_data_response.


    TYPES:
      BEGIN OF ty_erro,
        mensagem TYPE char100,
      END OF ty_erro.

    DATA: it_mensagem TYPE TABLE OF  ty_erro,
          wa_mensagem TYPE  ty_erro.

    CLEAR: r_msg_erro.

    IF ( me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'POST' AND  me->zif_integracao_inject~at_info_request_http-ds_metodo NE 'PUT' ).
      r_msg_erro = 'Metodo informado não reconhecido!'.
      e_status_code  = '405'. "Method Not Allowed
      RETURN.
    ENDIF.

    IF i_data_inbound IS INITIAL.
      r_msg_erro = 'Payload Requisição não pode ser vazio!'.
      e_status_code = '402'. "Payment Required
      RETURN.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).


    "FIELD-SYMBOLS: <fs_erro> TYPE lit_data_inbound.

    LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

      APPEND INITIAL LINE TO lwa_data_response_erro-erros ASSIGNING FIELD-SYMBOL(<fs_erro>).
*
*      "IDOrigem
      IF lwa_data_inbound-idorigem IS INITIAL.
        wa_mensagem = 'idOrigem deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        <fs_erro>-idorigem = lwa_data_inbound-idorigem.
      ENDIF.

      "OrigemCad
      IF lwa_data_inbound-origemcadastro IS INITIAL.
        wa_mensagem = 'origemCadastro deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSEIF lwa_data_inbound-origemcadastro NE 'EC'.
        wa_mensagem = 'origemCadastro incorreto'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ENDIF.
*
*      "genero
      IF lwa_data_inbound-genero IS NOT INITIAL AND ( lwa_data_inbound-genero NE 'MASCULINO' AND lwa_data_inbound-genero NE 'FEMININO'  AND lwa_data_inbound-genero NE 'EMPRESA' ).
        wa_mensagem = 'genero deve ser MASCULINO, FEMININO ou EMPRESA'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ENDIF.
*
*      "dtNascimento
      IF lwa_data_inbound-datanascimento IS NOT INITIAL.
        "validar formato
        DATA(length2) = strlen( lwa_data_inbound-datanascimento ).
        IF length2 <> 10.
          wa_mensagem = 'dataNascimento deve estar no formato YYYY-MM-DD.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: wa_mensagem.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento(4)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento+5(2)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-datanascimento+8(2)
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'dataNascimento deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          IF lwa_data_inbound-datanascimento+4(1) <> '-' OR lwa_data_inbound-datanascimento+7(1) <> '-'.
            wa_mensagem =  'dataNascimento  deve estar no formato YYYY-MM-DD.'.
            l_erro = 'X'.
            "APPEND wa_mensagem TO <fs_erro>-erros.
          ENDIF.

          IF wa_mensagem IS NOT INITIAL.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
        "  wa_mensagem =  'Data está no formato incorreto (formato esperado (YYYY-MM-DD)'.
      ENDIF.
*
*      "CNPJ
      IF lwa_data_inbound-cnpj IS NOT INITIAL AND lwa_data_inbound-cnpj  CA '/.,:;"[]\{}|<>?/'.
        wa_mensagem =  'cnpj deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.

        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-cnpj
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.
          wa_mensagem =  'cnpj deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.
      ENDIF.
*
*      "CPF
      IF lwa_data_inbound-cpf IS NOT INITIAL AND lwa_data_inbound-cpf  CA '/.,:;"[]\{}|<>?/'.

        wa_mensagem = 'cpf deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-cpf
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.

          wa_mensagem = 'cpf deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.

        ENDIF.
      ENDIF.
**
*      "Inscrição Estadual
      IF ( lwa_data_inbound-grupoconta EQ 'ZCPF' OR  lwa_data_inbound-grupoconta EQ 'ZCPJ' OR lwa_data_inbound-grupoconta EQ 'ZCFJ' OR lwa_data_inbound-grupoconta EQ 'ZCNJ' )
        AND lwa_data_inbound-inscestadual IS INITIAL.
        wa_mensagem =  'inscEstadual deve estar preenchido.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        IF lwa_data_inbound-inscestadual IS NOT INITIAL AND lwa_data_inbound-inscestadual  CA '/.,:;"[]\{}|<>?/'.
          wa_mensagem =  'inscEstadual deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-inscestadual
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'inscEstadual deve ser informado apenas os números.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
*
*
*      " InscricaoMunicipal
      IF lwa_data_inbound-inscmunicipal IS NOT INITIAL AND lwa_data_inbound-inscmunicipal  CA '/.,:;"[]\{}|<>?/'.
        wa_mensagem =  'inscMunicipal deve ser informado apenas os números.'.
        APPEND wa_mensagem TO <fs_erro>-erros.
        l_erro = 'X'.
      ELSE.
        CLEAR: lva_type.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = lwa_data_inbound-inscmunicipal
          IMPORTING
            htype     = lva_type.

        IF lva_type NE 'NUMC'.
          wa_mensagem =  'inscMunicipal deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.
      ENDIF.
*
*      "rg
      IF lwa_data_inbound-rg IS NOT INITIAL.
        IF lwa_data_inbound-rg  CA '/.,:;"[]\{}|<>?/'.
          wa_mensagem =  'rg  deve ser informado apenas os números.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          CLEAR: lva_type.
          CALL FUNCTION 'NUMERIC_CHECK'
            EXPORTING
              string_in = lwa_data_inbound-rg
            IMPORTING
              htype     = lva_type.

          IF lva_type NE 'NUMC'.
            wa_mensagem =  'rg deve ser informado apenas os números.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.

        ENDIF.

        IF lwa_data_inbound-orgexp IS INITIAL.
          wa_mensagem =  'orgexp deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        IF lwa_data_inbound-ufexp IS INITIAL.
          wa_mensagem =  'ufexp deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          "verifica se a UF está cadastrada no SAP.Tabela T005S-BLAND
          SELECT SINGLE * FROM t005s
            INTO @DATA(wl_t005s)
            WHERE bland = @lwa_data_inbound-ufexp
            and land1 eq @lwa_data_inbound-pais.
          IF sy-subrc IS NOT INITIAL.
            CONCATENATE 'ufexp não cadastrado nos dados mestres do sistema SAP para o País' lwa_data_inbound-pais into wa_mensagem SEPARATED BY space.

            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
*
*
      IF me->zif_integracao_inject~at_info_request_http-ds_metodo EQ 'POST'.

        "GrupoConta
        IF lwa_data_inbound-grupoconta IS INITIAL.
          wa_mensagem =  'grupoConta deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE. "validar se o que foi informado está na tabela do SAP Tabela T077X-KTOKD
          SELECT SINGLE * FROM  t077x
            INTO @DATA(wl_t077x)
            WHERE ktokd = @lwa_data_inbound-grupoconta.

          IF sy-subrc IS NOT INITIAL.
            wa_mensagem =   'grupoConta não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "Empresa
        IF lwa_data_inbound-empresa IS INITIAL.
          wa_mensagem =  'empresa deve estar preenchida.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.

        ELSE . "validar se o que foi informado está na tabela do SAP Tabela T001-BUKRS
          SELECT SINGLE * FROM  t001
            INTO @DATA(wl_t001)
            WHERE bukrs = @lwa_data_inbound-empresa.

          IF sy-subrc IS NOT INITIAL.

            wa_mensagem =  'empresa não cadastrada nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "SetorAtividade
        IF lwa_data_inbound-setoratividade IS NOT INITIAL.
          " validar se o que foi informado está na tabela do SAP TSPA-SPART

          SELECT SINGLE * FROM  tspa
            INTO @DATA(wl_tspa)
            WHERE spart = @lwa_data_inbound-setoratividade.

          IF sy-subrc IS NOT INITIAL.

            wa_mensagem =  'setorAtividade não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

        "nome
        IF lwa_data_inbound-nome IS INITIAL.
          wa_mensagem =  'nome deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        " endereco
        IF lwa_data_inbound-endereco IS INITIAL .
          wa_mensagem =  'endereco deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "cidade
        IF lwa_data_inbound-cidade IS INITIAL .
          wa_mensagem =  'cidade deve estar preenchida.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "bairro
        IF lwa_data_inbound-bairro  IS INITIAL .
          wa_mensagem =  'bairro deve estar preenchido.' .
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ENDIF.

        "uf
        IF lwa_data_inbound-uf  IS INITIAL.
          wa_mensagem =  'uf deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u)
             WHERE spras = @sy-langu
               AND land1 = @lwa_data_inbound-pais
               AND bland = @lwa_data_inbound-uf.

          IF sy-subrc <> 0.
            " concatenate   'Código da região incorreto '  lwa_data_inbound-uf  'para pais' lwa_data_inbound-pais into wa_mensagem .
            "wa_mensagem = 'uf não cadastrado nos dados mestres do sistema SAP.'.
            CONCATENATE 'uf não cadastrado nos dados mestres do sistema SAP para o País' lwa_data_inbound-pais into wa_mensagem SEPARATED BY space.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.

          ENDIF.
        ENDIF.

        "Cep
        IF lwa_data_inbound-cep  IS INITIAL.
          wa_mensagem =  'cep deve ser informado.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          DATA(length) = strlen( lwa_data_inbound-cep ).
          IF length <> 9.
            "CONCATENATE 'Código Postal'   lwa_data_inbound-cep  'deve ter comprimento 9' into wa_mensagem .
            wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ELSE.
            CLEAR wa_mensagem.
            IF lwa_data_inbound-cep+5(1) <> '-'.
              wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep(5)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep+6(3)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            IF wa_mensagem IS NOT INITIAL.
              APPEND wa_mensagem TO <fs_erro>-erros.
              l_erro = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

        "Pais
        IF lwa_data_inbound-pais  IS INITIAL.
          wa_mensagem =  'pais deve estar preenchido'.".'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          SELECT * FROM t005t
            INTO TABLE @DATA(lit_t005)
             WHERE spras EQ @sy-langu
               AND land1 EQ @lwa_data_inbound-pais.

          IF sy-subrc <> 0.
            wa_mensagem =  'pais não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.

      "-------------------------- PUT ------------------------------

      IF me->zif_integracao_inject~at_info_request_http-ds_metodo EQ 'PUT'.

        "Uf
        IF lwa_data_inbound-uf  IS NOT INITIAL.

          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u_1)
             WHERE spras = @sy-langu
               "AND land1 = @lwa_data_inbound-pais
               AND bland = @lwa_data_inbound-uf.

          IF sy-subrc <> 0.
            "concatenate  'Código da região incorreto '  lwa_data_inbound-uf  'para pais'  lwa_data_inbound-pais into wa_mensagem.
            wa_mensagem = 'uf não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.

          ENDIF.
        ENDIF.

        "Cep
        IF lwa_data_inbound-cep  IS INITIAL.
          wa_mensagem =  'cep deve ser informado.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.

          DATA(length3) = strlen( lwa_data_inbound-cep ).
          IF length3 <> 9.
            "CONCATENATE 'Código Postal'   lwa_data_inbound-cep  'deve ter comprimento 9' into wa_mensagem .
            wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ELSE.
            CLEAR wa_mensagem.
            IF lwa_data_inbound-cep+5(1) <> '-'.
              wa_mensagem = 'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep(5)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = lwa_data_inbound-cep+6(3)
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              wa_mensagem =  'cep  deve estar no formato XXXXX-XXX.'.
              "APPEND wa_mensagem TO <fs_erro>-erros.
            ENDIF.

            IF wa_mensagem IS NOT INITIAL.
              APPEND wa_mensagem TO <fs_erro>-erros.
              l_erro = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.

        "Pais
        IF lwa_data_inbound-pais  IS NOT INITIAL.
          "validar se está cadastrado nno SAP T005-LAND1
          SELECT SINGLE * FROM t005
            INTO @DATA(t_kna1)
            WHERE land1 = @lwa_data_inbound-pais.
          IF sy-subrc IS NOT INITIAL.
            wa_mensagem =  'pais não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.

        ENDIF.

        "ID Parceiro SAP
        IF lwa_data_inbound-idpartnersap IS INITIAL.
          wa_mensagem =  'idPartnerSap deve estar preenchido.'.
          APPEND wa_mensagem TO <fs_erro>-erros.
          l_erro = 'X'.
        ELSE.
          "valida se tem no SAP
          SELECT SINGLE * FROM kna1
            INTO @DATA(t_kna11)
            WHERE kunnr = @lwa_data_inbound-idpartnersap.
          IF sy-subrc IS NOT INITIAL.
            wa_mensagem = 'idPartnerSap não cadastrado nos dados mestres do sistema SAP.'.
            APPEND wa_mensagem TO <fs_erro>-erros.
            l_erro = 'X'.
          ENDIF.
        ENDIF.

      ENDIF.

*      IF wa_mensagem IS NOT INITIAL.
*        l_erro = 'X'.
*      ENDIF.


**** Código do cliente que chegou já existe.
**      IF lwa_data_inbound-idpartnersap IS NOT INITIAL.
**
**        SELECT SINGLE *
**          FROM kna1
**          INTO @DATA(lit_kna1)
**          WHERE kunnr EQ @lwa_data_inbound-idpartnersap .
**
**        IF sy-subrc = 0.
**          CONCATENATE 'Codigo cliente incorreto' lwa_data_inbound-idpartnersap
**                              INTO r_msg_erro  SEPARATED BY space.
**
**          w_tx-id_integracao     = i_id_integracao.
**          w_tx-id_origem           = lwa_data_inbound-idorigem.
**          w_tx-origem_cadastro   = 'EC'.
**          w_tx-msg_processamento = r_msg_erro.
**          w_tx-status_proc       = 'E'.
**          w_tx-id_cli_processado = ''.
**
**          MODIFY zsdt0327tx FROM w_tx.
**          CLEAR: w_tx.
**          COMMIT WORK.
**
**        ENDIF.
**      ENDIF.

*
    ENDLOOP.


    IF l_erro IS NOT INITIAL.
      r_msg_erro = /ui2/cl_json=>serialize( EXPORTING data = lwa_data_response_erro ).
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.

    r_if_integracao_inject = me.

  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_FORM_REQUEST_HTTP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    r_if_integracao_inject = me.

    SELECT  *
      FROM zsdt0327tx
      INTO TABLE @DATA(lit_zsdt0327tx)
      WHERE id_integracao =  @i_id_integracao
       AND status_proc = 'S'.

*** Classe que devolve o código do cliente:
    CHECK sy-subrc = 0.
    LOOP AT lit_zsdt0327tx INTO DATA(lwa_zsdt0327tx).
      CASE lwa_zsdt0327tx-origem_cadastro.
        WHEN 'EC'. "Ecommerce

          zcl_int_ob_cliente=>zif_int_ob_cliente~get_instance(
             )->set_cliente_confirmar( EXPORTING
                i_id_cliente   = lwa_zsdt0327tx-id_cli_processado
                i_id_origem    = lwa_zsdt0327tx-id_origem ) .

          e_sucesso =  abap_true.

        WHEN 'SF'. "Softexpert
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.

    r_if_integracao_inject = me.
    e_sucesso = abap_true.

    DATA: lit_data_inbound TYPE zsdt0325_inbcli_t,
          l_tipo_msg       TYPE bapi_mtype,
          l_mesg           TYPE string.

    DATA: wa_proc_ret    TYPE zsdt0318.

    DATA: wa_regcity TYPE j_1btreg_city,
          it_regcity LIKE STANDARD TABLE OF wa_regcity.

    DATA: lva_country    TYPE j_1btreg_city-country,
          lva_region     TYPE j_1btreg_city-region,
          lva_pstcd_from TYPE j_1btreg_city-pstcd_from,
          lva_pstcd_to   TYPE j_1btreg_city-pstcd_to,
          lva_erro(1)    TYPE c.

    DATA: wa_tx           TYPE zsdt0327tx.

    DATA: lc_msg_ret_canc TYPE string,
          lc_data_view    TYPE string,
          lc_erro         TYPE char1.

    DATA: lva_kna1       TYPE kna1,
          lva_duplic_flg TYPE c,
          lva_up_cpf     TYPE c,
          lva_kunnret    TYPE kna1-kunnr,
          lva_metodo     TYPE zintegracao-ds_metodo,
          lc_texto       TYPE string.

    DATA: e_json TYPE string.

    FIELD-SYMBOLS <icone>    LIKE icon_checked.

    DATA: lva_type TYPE dd01v-datatype.

*----------------------------------------------------------------------*
* Declaração para função SD_CUSTOMER_MAINTAIN_ALL
*----------------------------------------------------------------------*
    DATA: vg_kna1       TYPE kna1,
          vg_knb1       TYPE knb1,
          vg_knvv       TYPE knvv,
          vg_knvk       TYPE knvk ,          " Interface externa: dados detalhes bancários
          vg_bapiaddr1  TYPE bapiaddr1,
          vg_bapiaddr2  TYPE bapiaddr2,
          vg_kunnr      TYPE kna1-kunnr,
          vg_return     TYPE kna1-kunnr,
          vg_tabix      TYPE sy-tabix,
          vg_duplic_flg TYPE c,
          vg_kunnret    TYPE kna1-kunnr,
          vg_integra    TYPE zintegracao.

*----------------------------------------------------------------------*
* Declaração para função BP
*----------------------------------------------------------------------*
    CONSTANTS c_customer_role_0 TYPE bu_role   VALUE 'FLCU00'.
    CONSTANTS c_customer_role_1 TYPE bu_role   VALUE 'FLCU01'.
    CONSTANTS c_valid_to        TYPE datum     VALUE '99991231'.
    CONSTANTS c_test            TYPE abap_bool VALUE ''.

    DATA:
      gs_kna1       TYPE kna1  ,         " Mestre de fornecedores (parte geral)
      "gs_knvk       TYPE knvk ,          " Interface externa: dados detalhes bancários
      gt_knb1       TYPE cvis_knb1_t  ,  " Mestre de fornecedores (empresa)
      gt_knbw       TYPE cvis_knbw_t  ,  " Atual.dados mestre fornecedor: campos de tela e operativos
      gt_knvi       TYPE cvis_knvi_t  ,  " Mestre de clientes - indicadores de impostos
      wa_knvi       TYPE knvi,
      gt_knvv       TYPE cvis_knvv_t  ,  " Mestre de clientes (área de vendas)
      gt_knbk       TYPE knbk_t,
      gt_adsmtp     TYPE bbpt_er_adsmtp, " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
      lwa_adsmtp    LIKE LINE OF gt_adsmtp,
      gs_addr1_data TYPE addr1_data ,    " Estrutura de transferência para endereços
      gs_addr2_data TYPE addr2_data,     " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
      gs_sza1_d0100 TYPE sza1_d0100 .    " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)

    DATA: lva_bu_group   TYPE  tbd001-bu_group,
          lva_bu_partner TYPE bu_partner.

    DATA  lt_return_map TYPE mdg_bs_bp_msgmap_t.
    DATA  lt_return     TYPE bapiretm.
    DATA: et_return TYPE bapiret2_t.

    DATA: lv_msg         TYPE bapi_msg,
          lv_bu_type     TYPE bu_type,
          lv_object_task TYPE bus_ei_object_task. "I = Include (New); U = Update (Change)

    DATA: lt_bp_data TYPE cvis_ei_extern_t,
          lt_rules   TYPE TABLE OF bapibus1006_bproles.


    DATA: vg_message(220)  TYPE c,
          vg_mess_tab(256) TYPE c.

    r_if_integracao_inject = me.

    CHECK i_msg IS NOT INITIAL.

    /ui2/cl_json=>deserialize( EXPORTING json = i_msg CHANGING data = lit_data_inbound ).

    CLEAR: lva_erro.

    IF lit_data_inbound[] IS NOT INITIAL.

      LOOP AT  lit_data_inbound INTO DATA(w_data_inbound).

        CLEAR: vg_integra, lva_metodo.
        SELECT SINGLE *
          FROM zintegracao
        INTO CORRESPONDING FIELDS OF vg_integra
          WHERE id_integracao EQ i_id_integracao
             AND id_interface = '156'.
        IF sy-subrc = 0.
          lva_metodo = vg_integra-ds_metodo.
        ELSE.
          wa_tx-id_integracao     = i_id_integracao.
          wa_tx-id_origem         = w_data_inbound-idorigem.
          wa_tx-origem_cadastro   = 'EC'.
          wa_tx-msg_processamento = 'Método Integração não encontrado'.
          wa_tx-status_proc       = 'E'.
          wa_tx-id_cli_processado = ''.
          wa_tx-dt_registro = sy-datum.
          wa_tx-hr_registro = sy-uzeit.
          wa_tx-us_registro = sy-uname.

          MODIFY zsdt0327tx FROM wa_tx.
          CLEAR: wa_tx.
          COMMIT WORK.
          lva_erro = 'X'.
          CONTINUE.
        ENDIF.
* VALIDAR CPF / RG / INSCRICAO COM CARACTERES ESPECIAIS
        IF w_data_inbound-cpf IS NOT  INITIAL.
          IF w_data_inbound-cpf CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o CPF:'  w_data_inbound-cpf
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-cpf
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo CPF:'  w_data_inbound-cpf
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.
              wa_tx-dt_registro       = sy-datum.
              wa_tx-hr_registro       = sy-uzeit.
              wa_tx-us_registro       = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF  w_data_inbound-cnpj IS NOT INITIAL.
          IF w_data_inbound-cnpj  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o CNPJ:'  w_data_inbound-cnpj
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-cnpj
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo CNPJ:'  w_data_inbound-cnpj
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.
              wa_tx-dt_registro = sy-datum.
              wa_tx-hr_registro = sy-uzeit.
              wa_tx-us_registro = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-rg IS NOT INITIAL.
          IF w_data_inbound-rg  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para o RG:'  w_data_inbound-rg
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-rg
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo RG:'  w_data_inbound-rg
                 INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.
              wa_tx-dt_registro = sy-datum.
              wa_tx-hr_registro = sy-uzeit.
              wa_tx-us_registro = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-inscestadual IS NOT INITIAL.
          IF w_data_inbound-inscestadual CA '/.,:;"[]\{}|<>?/'.

            CONCATENATE 'Caractere Especial não é permitido para Insc.Estadual:' w_data_inbound-inscestadual
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-inscestadual
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.
              CONCATENATE 'String não permitido no campo Insc.Estadual:'  w_data_inbound-inscestadual
               INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.
              wa_tx-dt_registro = sy-datum.
              wa_tx-hr_registro = sy-uzeit.
              wa_tx-us_registro = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

        IF w_data_inbound-inscmunicipal IS NOT INITIAL.
          IF w_data_inbound-inscmunicipal  CA '/.,:;"[]\{}|<>?/'.
            CONCATENATE 'Caractere Especial não é permitido para Insc.Municipal:'  w_data_inbound-inscmunicipal
            INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ELSE.
            CLEAR: lva_type.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                string_in = w_data_inbound-inscmunicipal
              IMPORTING
                htype     = lva_type.

            IF lva_type NE 'NUMC'.

              CONCATENATE 'String não permitido no campo Insc.Municipal:' w_data_inbound-inscmunicipal
               INTO vg_message  SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = ''.
              wa_tx-dt_registro = sy-datum.
              wa_tx-hr_registro = sy-uzeit.
              wa_tx-us_registro = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
              lva_erro = 'X'.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.

* Validar CEP
        IF    w_data_inbound-cep IS NOT INITIAL.

          DATA(length) = strlen( w_data_inbound-cep ).
          IF length <> 9.
            CONCATENATE 'Código Postal' w_data_inbound-cep 'deve ter comprimento 9'
               INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro = sy-datum.
            wa_tx-hr_registro = sy-uzeit.
            wa_tx-us_registro = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.

* Validar PAIS
        IF w_data_inbound-pais IS NOT INITIAL.
          SELECT * FROM t005t
            INTO TABLE @DATA(lit_t005)
             WHERE spras EQ @sy-langu
               AND land1 EQ @w_data_inbound-pais.

          IF sy-subrc <> 0.
            CONCATENATE 'Código do Pais incorreto' w_data_inbound-pais
              INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro       = sy-datum.
            wa_tx-hr_registro       = sy-uzeit.
            wa_tx-us_registro       = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.
* Validar UF
        IF w_data_inbound-uf IS NOT INITIAL.

          SELECT  * FROM t005u
            INTO TABLE @DATA(lit_t005u)
             WHERE spras = @sy-langu
               AND land1 = @w_data_inbound-pais
               AND bland = @w_data_inbound-uf.

          IF sy-subrc <> 0.
            CONCATENATE 'Código da região incorreto ' w_data_inbound-uf 'para pais' w_data_inbound-pais
              INTO vg_message  SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro       = sy-datum.
            wa_tx-hr_registro       = sy-uzeit.
            wa_tx-us_registro       = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.
            CONTINUE.
          ENDIF.
        ENDIF.
*--------------------------------------
*-- INSERIR
*--------------------------------------
        IF lva_metodo = 'POST'.
*-----------------------------------------------------------------------------------------------------------------------*
*     Valida CPF / CNJ / Inscrição Estadual
*-----------------------------------------------------------------------------------------------------------------------*
          CLEAR: lva_kna1, lva_duplic_flg,lva_kunnret, lva_up_cpf.

          "lva_kna1-kunnr = w_data_inbound-idpartnersap.
          lva_kna1-stcd1 = w_data_inbound-cnpj.
          lva_kna1-stcd2 = w_data_inbound-cpf.
          lva_kna1-stcd3 = w_data_inbound-inscestadual.
          lva_kna1-ktokd = w_data_inbound-grupoconta.

          IF w_data_inbound-rg IS NOT  INITIAL.
            lva_kna1-stcd4 = w_data_inbound-inscmunicipal.
            CONCATENATE w_data_inbound-rg '-' w_data_inbound-orgexp '/' w_data_inbound-ufexp INTO DATA(lva_rg).
            CONDENSE lva_rg NO-GAPS.
            lva_kna1-stcd4 = lva_rg.
          ENDIF.

          me->mt_consist_tax(
          EXPORTING
            im_kunnr = lva_kna1-kunnr
            im_stcd1 = lva_kna1-stcd1
            im_stcd2 = lva_kna1-stcd2
            im_stcd3 = lva_kna1-stcd3
            im_ktokd = lva_kna1-ktokd
            CHANGING
              cs_duplic_flg = lva_duplic_flg
              cs_kunnret    = lva_kunnret
              ).

          IF ( NOT lva_duplic_flg IS INITIAL ).
            lva_up_cpf = 'X'.

            CONCATENATE 'Duplicidade de CNPJ ou CPF com o cliente' lva_kunnret
                                  INTO vg_message SEPARATED BY space.

            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-id_origem         =  w_data_inbound-idorigem.
            wa_tx-msg_processamento = vg_message.
            wa_tx-status_proc       = 'S'.
            wa_tx-id_cli_processado = lva_kunnret.
            wa_tx-dt_registro       = sy-datum.
            wa_tx-hr_registro       = sy-uzeit.
            wa_tx-us_registro       = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.

          ELSE.

            CLEAR: lva_country,
                   lva_region  ,
                   lva_pstcd_from,
                   lva_pstcd_to.

            lva_country      = w_data_inbound-pais.
            lva_region       = w_data_inbound-uf.
            lva_pstcd_from   = w_data_inbound-cep.
            lva_pstcd_to     = w_data_inbound-cep.

            CLEAR: it_regcity.
            SELECT * FROM j_1btreg_city
                   INTO TABLE it_regcity
                  WHERE ( country    EQ  lva_country  )
                    AND ( region     EQ  lva_region   )
                    AND ( pstcd_from LE  lva_pstcd_from )
                    AND ( pstcd_to   GE  lva_pstcd_to   ).

***  VERIFICO SE É PESSOA FÍSICA OU JURIDICA.
            CLEAR: vg_kna1.
            IF w_data_inbound-grupoconta = 'ZCPF'." - CLIENTE PRODUTOR PESSOA FISICA
              vg_kna1-stkzn = 'X'.
              IF w_data_inbound-genero = 'MASCULINO'.
                gs_addr2_data-title_p = '0002'.
                gs_addr1_data-title   = '0002'.
              ELSE.
                gs_addr2_data-title_p  = '0001'.
                gs_addr1_data-title    = '0001'.
              ENDIF.
            ELSE.
              IF w_data_inbound-grupoconta = 'ZCPJ'. " - CLIENTE PRODUTOR PESSOA JURIDICA
                vg_kna1-stkzn = ''.
                gs_addr2_data-title_p = '0003'.
                gs_addr1_data-title   = '0003'.
              ELSE.
                gs_addr1_data-title   = ''.
                gs_addr2_data-title_p = ''.
              ENDIF.
            ENDIF.
* PARAMETRIZAÇÃO

            SELECT * FROM zsdt0317
              INTO TABLE @DATA(it_zsdt0317)
              WHERE cancelado <> 'X'
               AND ktokd = @w_data_inbound-grupoconta
               AND bukrs = @w_data_inbound-empresa.

            SELECT * FROM zsdt0319
              INTO TABLE @DATA(it_zsdt0319)
              FOR ALL ENTRIES IN @it_zsdt0317
            WHERE cancelado <> 'X'
              AND id = @it_zsdt0317-id .

            SELECT * FROM zsdt0320
              INTO TABLE @DATA(it_zsdt0320)
              FOR ALL ENTRIES IN @it_zsdt0319
              WHERE id =  @it_zsdt0319-id
               AND seq_canal = @it_zsdt0319-seq_canal
               AND cancelado <> 'X'.

            SELECT * FROM zsdt0322
              INTO TABLE @DATA(it_zsdt0322)
              FOR ALL ENTRIES IN @it_zsdt0319
              WHERE id =  @it_zsdt0319-id
               AND  seq_canal = @it_zsdt0319-seq_canal
               AND  cancelado <> 'X'.

* MESTRE DE CLIENTES (PARTE GERAL)
            vg_kna1-ktokd    = w_data_inbound-grupoconta .
            vg_kna1-land1    = w_data_inbound-pais.
            vg_kna1-sortl    = w_data_inbound-nome+0(10).
            vg_kna1-name1    = w_data_inbound-nome.
            vg_kna1-name2    = w_data_inbound-nome+0(10).
            vg_kna1-ort01    = w_data_inbound-cidade.
            vg_kna1-pstlz    = w_data_inbound-cep.
            vg_kna1-regio    = w_data_inbound-uf.
            vg_kna1-stras    = w_data_inbound-endereco.

            vg_kna1-anred    = 'PT.'.
            vg_kna1-spras    = sy-langu.
            vg_kna1-erdat    = sy-datum.
            vg_kna1-ernam    = sy-uname.
            vg_kna1-stcd1    = w_data_inbound-cnpj.
            vg_kna1-stcd2    = w_data_inbound-cpf.
            vg_kna1-stcd3    = w_data_inbound-inscestadual.
            "vg_kna1-stcd4    = w_data_inbound-inscmunicipal.
            vg_kna1-stcd4    = lva_kna1-stcd4. " w_data_inbound-inscmunicipal.
            vg_kna1-telf1    = w_data_inbound-telefonefixo.
            vg_kna1-telf2    = w_data_inbound-telefonemovel.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_kna1-telfx.

* MESTRE DE CLIENTES (EMPRESA)
* Cabeçalho dos Parâmetros para o Cadastro Cliente
            SORT it_zsdt0317 BY bukrs.
            LOOP AT it_zsdt0317 INTO DATA(lwa_zsdt0317).

              CLEAR: vg_knb1.
              vg_knb1-bukrs = lwa_zsdt0317-bukrs.
              vg_knb1-akont = lwa_zsdt0317-akont.
              vg_knb1-zuawa = lwa_zsdt0317-zuawa.
              vg_knb1-fdgrv = lwa_zsdt0317-fdgrv.
              vg_knb1-zterm = lwa_zsdt0317-zterm.
              vg_knb1-zwels = lwa_zsdt0317-zwels.

              APPEND vg_knb1 TO gt_knb1.

              LOOP AT it_zsdt0319 INTO DATA(lwa_zsdt0319) WHERE id = lwa_zsdt0317-id.

* MESTRE DE CLIENTES: DADOS DE VENDAS E DISTRIBUIÇÃO
                CLEAR: vg_knvv.
                vg_knvv-vkorg    = lwa_zsdt0317-bukrs.
                vg_knvv-vtweg    = lwa_zsdt0319-vtweg.

                LOOP AT it_zsdt0320 INTO DATA(lwa_zsdt0320) WHERE id = lwa_zsdt0319-id
                                                             AND  seq_canal = lwa_zsdt0319-seq_canal .

                  READ TABLE it_zsdt0322 INTO DATA(lwa_zsdt0322) WITH KEY id = lwa_zsdt0319-id
                                                                   seq_canal = lwa_zsdt0319-seq_canal.
                  vg_knvv-spart    = lwa_zsdt0320-spart.
                  vg_knvv-kalks    = lwa_zsdt0322-kalks.
                  vg_knvv-kdgrp    = '7'.
                  vg_knvv-waers    = lwa_zsdt0322-waers.
                  vg_knvv-ktgrd    = lwa_zsdt0322-ktgrd.
                  vg_knvv-versg    = lwa_zsdt0322-versg.
                  vg_knvv-lprio    = lwa_zsdt0322-lprio.
                  vg_knvv-vsbed    = lwa_zsdt0322-vsbed.
                  vg_knvv-kzazu    = lwa_zsdt0322-kzazu.
                  vg_knvv-kztlf    = lwa_zsdt0322-kztlf.
                  vg_knvv-perfk    = lwa_zsdt0322-perfk.

                  APPEND vg_knvv TO gt_knvv.

* MESTRE DE CLIENTES - INDICADORES DE IMPOSTOS
                  CLEAR: gt_knvi[], wa_knvi.
                  IF vg_kunnr IS NOT INITIAL.
                    wa_knvi-kunnr = vg_kunnr.
                  ENDIF.

                  wa_knvi-aland = 'BR'.               "tax country
                  wa_knvi-tatyp = lwa_zsdt0322-tatyp. "tax category
                  wa_knvi-taxkd = lwa_zsdt0322-taxkd. "tax classification
                  APPEND wa_knvi TO gt_knvi.
                  CLEAR:  wa_knvi.

                ENDLOOP.
              ENDLOOP.
            ENDLOOP.

* ESTRUTURA DE REFERÊNCIA BAPI PARA ENDEREÇOS (ORG./FIRMA)
            gs_addr1_data-sort1        = w_data_inbound-nome+0(10).
            gs_addr1_data-name1        = w_data_inbound-nome.
            gs_addr1_data-city1        = w_data_inbound-cidade.
            gs_addr1_data-post_code1   = w_data_inbound-cep.
            gs_addr1_data-street       = w_data_inbound-endereco.
            gs_addr1_data-house_num1   = w_data_inbound-nrendereco.
            gs_addr1_data-city2        = w_data_inbound-bairro.
            gs_addr1_data-country      = w_data_inbound-pais.
            gs_addr1_data-langu        = sy-langu.
            gs_addr1_data-region       = w_data_inbound-uf.

            gs_sza1_d0100-tel_number = w_data_inbound-telefonefixo.
            gs_sza1_d0100-mob_number = w_data_inbound-telefonemovel.

            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO gs_sza1_d0100-fax_number.

            LOOP AT it_regcity INTO wa_regcity
                                 WHERE ( country    EQ lva_country        )
                                   AND ( region     EQ lva_region         )
                                   AND ( pstcd_from LE lva_pstcd_from     )
                                   AND ( pstcd_to   GE lva_pstcd_to       ).

              gs_addr1_data-taxjurcode = wa_regcity-taxjurcode.
              gs_addr1_data-taxjurcode = wa_regcity-taxjurcode.
              vg_kna1-locco = wa_regcity-taxjurcode.
              EXIT.
            ENDLOOP.

            gs_addr2_data-name_last   = w_data_inbound-nome.
            gs_addr2_data-name_text   = w_data_inbound-nome.
            gs_addr2_data-city1       = w_data_inbound-cidade.
            gs_addr2_data-post_code1  = w_data_inbound-cep.
            gs_addr2_data-street      = w_data_inbound-endereco.
            gs_addr2_data-country     = w_data_inbound-pais.
            gs_addr2_data-region      = w_data_inbound-uf.
            gs_addr2_data-langu_p     = sy-langu.
            gs_addr2_data-house_num1  = w_data_inbound-nrendereco.

            gs_sza1_d0100-smtp_addr = w_data_inbound-email.

            lwa_adsmtp-smtp_addr =   w_data_inbound-emailaux.

            APPEND lwa_adsmtp TO gt_adsmtp.

            vg_kna1-kunnr = vg_kunnr.
            vg_knb1-kunnr = vg_kunnr.
            vg_knb1-bukrs = lwa_zsdt0317-bukrs.

            REPLACE ALL OCCURRENCES OF '-' IN w_data_inbound-datanascimento WITH space.
            CONDENSE w_data_inbound-datanascimento NO-GAPS.
            vg_knvk-gbdat = w_data_inbound-datanascimento  .

************************************************************************************
            "SUPPLIER
            CLEAR: et_return, lt_bp_data.
            APPEND INITIAL LINE TO lt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>).
            "--------------------------------------------------------------------
            "Define se é uma Organização (2) ou uma Pessoa (1)
            lv_bu_type = COND #( WHEN vg_kna1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).
            lv_object_task = 'I'.

            "PARTNER - Header
            me->mt_fill_part_header_sup(
              EXPORTING
                im_object_task = lv_object_task       " Interface externa: código de modificação objeto
                im_partner     = space                " Nº parceiro de negócios
                im_guid        = space                " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "PARTNER - Central - Common
            me->mt_fill_part_cent_common_cust(
              EXPORTING
                im_object_task = lv_object_task
                im_bu_type     = lv_bu_type
                is_kna1        = vg_kna1
                is_addr1_data  = gs_addr1_data
              CHANGING
                cs_bp_data     = <fs_bp_data>
            ).

            "PARTNER - Central - Address
            me->mt_fill_part_cent_address_cust(
              EXPORTING
                im_object_task = lv_object_task      " Objects data
                is_addr1_data  = gs_addr1_data       " Estrutura de transferência para endereços
                is_sza1_d0100  = gs_sza1_d0100       " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
                is_kna1        = vg_kna1
                it_adsmtp      = gt_adsmtp           " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
            ).

            "PARTNER - Central - Tax
            me->mt_fill_part_cent_tax_cust(
              EXPORTING
                im_object_task = lv_object_task
                is_kna1        = vg_kna1
              CHANGING
                cs_bp_data     = <fs_bp_data>
            ).

            " PARTNER - Central - Roles
            me->mt_fill_part_cent_roles_sup(
              EXPORTING
                im_object_task = lv_object_task       " Interface externa: código de modificação objeto
                im_role        = c_customer_role_0    " Função de parceiro de negócios
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            me->mt_fill_part_cent_roles_sup(
              EXPORTING
                im_object_task = lv_object_task       " Interface externa: código de modificação objeto
                im_role        = c_customer_role_1    " Função de parceiro de negócios
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
              ).

            "Customer - Header
            me->mt_fill_part_cust_header(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação objeto
                im_create      = abap_true
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "Supplier - Data
            me->mt_fill_part_cust_data(
              EXPORTING
                is_kna1    = vg_kna1               " Atual.dados mestre fornecedor: campos de tela e operativos
              CHANGING
                cs_bp_data = <fs_bp_data>           " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "Supplier - Address
            me->mt_fill_part_cust_address(
              EXPORTING
                im_object_task = lv_object_task                " Interface externa: código de modificação objeto
                is_kna1        = vg_kna1                       " Mestre de fornecedores (parte geral)
                is_addr1_data  = gs_addr1_data                " Estrutura de transferência para endereços
              CHANGING
                cs_bp_data     = <fs_bp_data>                " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "Supplier - Contact
            me->mt_fill_part_cust_contact(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação objeto
                is_kna1        = vg_kna1
                is_sza1_d0100  = gs_sza1_d0100      " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
                is_knvk        = vg_knvk
                it_adsmtp      = gt_adsmtp          " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            me->mt_fill_part_cust_tax(
              EXPORTING
                im_object_task = lv_object_task
                it_knvi        = gt_knvi
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            me->mt_fill_part_cust_company(
              EXPORTING
                im_object_task = lv_object_task
                it_knb1        = gt_knb1
                it_knbw        = gt_knbw
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            me->mt_fill_part_cust_sales(
               EXPORTING
                 im_object_task = lv_object_task
                 it_knvv        = gt_knvv
               CHANGING
                 cs_bp_data     = <fs_bp_data>
             ).

            me->mt_fill_part_cust_bankdetail(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação dados bancários
                it_knbk        = gt_knbk            " Interface externa: dados detalhes bancários
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            UNASSIGN <fs_bp_data>.

            "Valida antes de efetivar alteração.
            DATA(ls_data) = lt_bp_data[ 1 ].
            cl_md_bp_maintain=>validate_single(
              EXPORTING
                i_data        = ls_data
              IMPORTING
                et_return_map = lt_return_map ).

            IF lt_return_map[] IS NOT INITIAL.
              LOOP AT lt_return_map ASSIGNING FIELD-SYMBOL(<fs_l_return_map>).
                APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<fs_return>).
                MOVE-CORRESPONDING <fs_l_return_map> TO <fs_return>.
                REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
                UNASSIGN <fs_return>.
              ENDLOOP.
              LOOP AT et_return INTO DATA(lwa_return).

                CONCATENATE 'Cliente não criado pelo motivo:!'  lwa_return-message
               INTO vg_message SEPARATED BY space.

                wa_tx-id_integracao     = i_id_integracao.
                wa_tx-origem_cadastro   = 'EC'.
                wa_tx-id_origem         = w_data_inbound-idorigem.
                wa_tx-id_erro           = lwa_return-number.
                wa_tx-msg_processamento =  vg_message.
                wa_tx-status_proc       = 'E'.
                wa_tx-id_cli_processado = vg_kunnr.
                wa_tx-dt_registro       = sy-datum.
                wa_tx-hr_registro       = sy-uzeit.
                wa_tx-us_registro       = sy-uname.

                MODIFY zsdt0327tx FROM wa_tx.
                CLEAR: wa_tx, vg_message.
                COMMIT WORK.
              ENDLOOP.
              RETURN.
            ENDIF.

*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
            "IMPORTANTE!
            "Se por algum motivo algum dado não está sendo preenchido mesmo sendo passado
            "para a classe, e nenhum erro está retornando, ver transação MDS_PPO2.
            "Foi nessa transação que eu vi que os dados da KNB1 não estavam sendo gravados
            "porque o campo KNA1-CFOPC não estava sendo informado.
            CALL METHOD cl_md_bp_maintain=>maintain
              EXPORTING
                i_data     = lt_bp_data
                i_test_run = c_test
              IMPORTING
                e_return   = lt_return.

            LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_l_return>).
              LOOP AT <fs_l_return>-object_msg ASSIGNING FIELD-SYMBOL(<fs_object_msg>).
                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return>.
                MOVE-CORRESPONDING <fs_object_msg> TO <fs_return>.
                REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
                UNASSIGN <fs_return>.
              ENDLOOP.
            ENDLOOP.

            IF line_exists( et_return[ type = 'E' ] ).
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              lva_erro = 'X'.

              LOOP AT et_return INTO lwa_return.
                CONCATENATE 'Cliente não criado pelo motivo:!'   lwa_return-message
                INTO vg_message SEPARATED BY space.

                wa_tx-id_integracao     = i_id_integracao.
                wa_tx-origem_cadastro   = 'EC'.
                wa_tx-id_origem         = w_data_inbound-idorigem.
                wa_tx-id_erro           = lwa_return-number.
                wa_tx-msg_processamento = vg_message.
                wa_tx-status_proc       = 'E'.
                wa_tx-id_cli_processado = vg_kunnr.
                wa_tx-dt_registro       = sy-datum.
                wa_tx-hr_registro       = sy-uzeit.
                wa_tx-us_registro       = sy-uname.

                MODIFY zsdt0327tx FROM wa_tx.
                CLEAR: wa_tx, lwa_return.
                COMMIT WORK.
              ENDLOOP.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              SELECT SINGLE partner
                FROM but000
              INTO @DATA(lv_partner)
                WHERE partner_guid = @ls_data-partner-header-object_instance-bpartnerguid.

              SELECT SINGLE * FROM ibupacustomer
                INTO @DATA(wa_ibupacustomer)
                WHERE businesspartner = @lv_partner
                AND businesspartneruuid = @ls_data-partner-header-object_instance-bpartnerguid. .

              w_data_inbound-idpartnersap = wa_ibupacustomer-customer.

              CONCATENATE 'Cliente' w_data_inbound-idpartnersap 'criado com sucesso!'
                                    INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'S'.
              wa_tx-id_cli_processado = w_data_inbound-idpartnersap.
              wa_tx-dt_registro       = sy-datum.
              wa_tx-hr_registro       = sy-uzeit.
              wa_tx-us_registro       = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDIF.
*** UPDATE
        IF ( lva_metodo = 'PUT' ) OR ( lva_up_cpf = 'X' ) .
          DATA: bapi_kunnr TYPE bapi4001_1-objkey.

          IF lva_metodo = 'PUT'.
            bapi_kunnr =  w_data_inbound-idpartnersap.
          ELSE.
            bapi_kunnr =  lva_kunnret.
          ENDIF.

          IF bapi_kunnr IS INITIAL.
            wa_tx-id_integracao     = i_id_integracao.
            wa_tx-id_origem         = w_data_inbound-idorigem.
            wa_tx-origem_cadastro   = 'EC'.
            wa_tx-msg_processamento = 'UPDATE - Código cliente não encontrado'.
            wa_tx-status_proc       = 'E'.
            wa_tx-id_cli_processado = ''.
            wa_tx-dt_registro       = sy-datum.
            wa_tx-hr_registro       = sy-uzeit.
            wa_tx-us_registro       = sy-uname.

            MODIFY zsdt0327tx FROM wa_tx.
            CLEAR: wa_tx.
            COMMIT WORK.
            lva_erro = 'X'.

          ELSE.

            CLEAR: vg_kna1.
            SELECT SINGLE *
              FROM kna1
              INTO CORRESPONDING FIELDS OF vg_kna1
              WHERE kunnr EQ bapi_kunnr.

            CLEAR: gs_addr1_data,
                   gs_addr2_data.

***  VERIFICO SE É PESSOA FÍSICA OU JURIDICA.
            IF w_data_inbound-grupoconta = 'ZCPF'." - CLIENTE PRODUTOR PESSOA FISICA
              vg_kna1-stkzn = 'X'.
              IF w_data_inbound-genero = 'MASCULINO'.
                gs_addr2_data-title_p = '0002'.
                gs_addr1_data-title   = '0002'.
              ELSE.
                gs_addr2_data-title_p  = '0001'.
                gs_addr1_data-title    = '0001'.
              ENDIF.
            ELSE.
              IF w_data_inbound-grupoconta = 'ZCPJ'. " - CLIENTE PRODUTOR PESSOA JURIDICA
                vg_kna1-stkzn = ''.
                gs_addr2_data-title_p = '0003'.
                gs_addr1_data-title   = '0003'.
              ELSE.
                gs_addr1_data-title   = ''.
                gs_addr2_data-title_p = ''.
              ENDIF.
            ENDIF.

*** Enviar mensagem avisando que é diferente o grupo de conta
            IF w_data_inbound-grupoconta <> vg_kna1-ktokd.
              CONCATENATE 'Cliente com grupo de conta diferente:!' w_data_inbound-grupoconta '/' vg_kna1-ktokd
                             INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-id_origem          =  w_data_inbound-idorigem.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = vg_kunnr.
              wa_tx-dt_registro       = sy-datum.
              wa_tx-hr_registro       = sy-uzeit.
              wa_tx-us_registro       = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
            ENDIF.

            SELECT SINGLE * FROM kna1 INTO vg_kna1 WHERE kunnr EQ w_data_inbound-idpartnersap.

            vg_kna1-name1   = w_data_inbound-nome.
            vg_kna1-ort02   = w_data_inbound-cidade.
            vg_kna1-pstlz   = w_data_inbound-cep.
            vg_kna1-regio   = w_data_inbound-uf.
            vg_kna1-stras   = w_data_inbound-endereco.
            vg_kna1-erdat   = sy-datum.
            vg_kna1-ernam   = sy-uname.
            vg_kna1-telf1 = w_data_inbound-telefonefixo.
            vg_kna1-telf2 = w_data_inbound-telefonemovel.
            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_kna1-telfx.

            "SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF vg_bapiaddr1 WHERE addrnumber = vg_kna1-adrnr AND date_to >= sy-datum.

            gs_addr1_data-sort1      = w_data_inbound-nome+0(10).
            gs_addr1_data-name1      = w_data_inbound-nome.
            gs_addr1_data-city1      = w_data_inbound-cidade.
            gs_addr1_data-post_code1 = w_data_inbound-cep.
            gs_addr1_data-street     = w_data_inbound-endereco.
            gs_addr1_data-house_num1 = w_data_inbound-nrendereco.
            gs_addr1_data-country    = w_data_inbound-pais.
            gs_addr1_data-langu      = sy-langu.
            gs_addr1_data-region     = w_data_inbound-uf.
            gs_addr1_data-city2      = w_data_inbound-bairro.

            gs_sza1_d0100-tel_number = w_data_inbound-telefonefixo.
            gs_sza1_d0100-mob_number = w_data_inbound-telefonemovel.

            CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO gs_sza1_d0100-fax_number.

            "SELECT SINGLE * FROM adrc INTO CORRESPONDING FIELDS OF vg_bapiaddr2 WHERE addrnumber = vg_kna1-adrnr AND date_to >= sy-datum.

            CLEAR: lva_country,
                   lva_region  ,
                   lva_pstcd_from,
                   lva_pstcd_to.

            lva_country      = w_data_inbound-pais.
            lva_region       = w_data_inbound-uf.
            lva_pstcd_from   = w_data_inbound-cep.
            lva_pstcd_to     = w_data_inbound-cep.

            CLEAR: it_regcity.
            SELECT * FROM j_1btreg_city
                   INTO TABLE it_regcity
                  WHERE ( country    EQ  lva_country  )
                    AND ( region     EQ  lva_region   )
                    AND ( pstcd_from LE  lva_pstcd_from )
                    AND ( pstcd_to   GE  lva_pstcd_to   ).

            LOOP AT it_regcity INTO wa_regcity
                                 WHERE ( country    EQ lva_country        )
                                   AND ( region     EQ lva_region         )
                                   AND ( pstcd_from LE lva_pstcd_from     )
                                   AND ( pstcd_to   GE lva_pstcd_to       ).

              gs_addr1_data-taxjurcode = wa_regcity-taxjurcode.
              gs_addr1_data-taxjurcode = wa_regcity-taxjurcode.
              vg_kna1-locco = wa_regcity-taxjurcode.
              EXIT.
            ENDLOOP.

            gs_addr2_data-name_last    = w_data_inbound-nome.
            gs_addr2_data-name_text    = w_data_inbound-nome.
            gs_addr2_data-city1        = w_data_inbound-cidade.
            gs_addr2_data-post_code1   = w_data_inbound-cep.
            gs_addr2_data-street       = w_data_inbound-endereco.
            gs_addr2_data-country      = w_data_inbound-pais.
            gs_addr2_data-region       = w_data_inbound-uf.
            gs_addr2_data-langu_p      = sy-langu.
            gs_addr2_data-city2        = w_data_inbound-bairro.
            gs_addr2_data-house_num1   = w_data_inbound-nrendereco.
            "CONCATENATE w_data_inbound-origemcadastro w_data_inbound-idorigem INTO vg_bapiaddr2-fax_number.
            "vg_bapiaddr2-tel1_numbr  = w_data_inbound-telefonefixo.

            gs_sza1_d0100-smtp_addr = w_data_inbound-email.

            lwa_adsmtp-smtp_addr =   w_data_inbound-emailaux.
            APPEND lwa_adsmtp TO gt_adsmtp.

************************************************************************************
            CLEAR:  et_return,lt_bp_data.

            APPEND INITIAL LINE TO lt_bp_data ASSIGNING <fs_bp_data>.

            "Define se é uma Organização (2) ou uma Pessoa (1)
            lv_bu_type = COND #( WHEN vg_kna1-stcd1 IS NOT INITIAL THEN '2' ELSE '1' ).
            lv_object_task = 'U'.

            SELECT SINGLE  *
              FROM ibupacustomer INTO @DATA(lwa_ibupacustomer)
            WHERE customer = @bapi_kunnr.

            IF sy-subrc NE 0  .

              CONCATENATE 'BP para cliente:!' bapi_kunnr 'não encontrada !'
                INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-id_origem         = w_data_inbound-idorigem.
              wa_tx-id_erro           = '0000'.
              wa_tx-msg_processamento =  vg_message.
              wa_tx-status_proc       = 'E'.
              wa_tx-id_cli_processado = bapi_kunnr.
              wa_tx-dt_registro       = sy-datum.
              wa_tx-hr_registro       = sy-uzeit.
              wa_tx-us_registro       = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx, vg_message.
              COMMIT WORK.
              RETURN.
            ELSE.
              DATA: gs_but000 TYPE but000.
              lva_bu_partner = lwa_ibupacustomer-businesspartner.

              CALL FUNCTION 'BUPA_NUMBERS_GET'
                EXPORTING
                  iv_partner = lwa_ibupacustomer-businesspartner     " Business Partner Number
                IMPORTING
                  es_but000  = gs_but000.   " Business Partner Data

              lv_bu_type = gs_but000-type.
            ENDIF.

            "Customer - Header
            me->mt_fill_part_cust_header(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação objeto
                im_create      = abap_true
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "PARTNER - Header
            me->mt_fill_part_header_sup(
              EXPORTING
                im_object_task = lv_object_task       " Interface externa: código de modificação objeto
                im_partner     = lva_bu_partner       " Nº parceiro de negócios
                im_guid        = space                " GUID de um parceiro de negócio em formato CHAR 32 para BAPI
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "PARTNER - Central - Common
            me->mt_fill_part_cent_common_cust(
              EXPORTING
                im_object_task = lv_object_task
                im_bu_type     = lv_bu_type
                is_kna1        = vg_kna1
                is_addr1_data  = gs_addr1_data
              CHANGING
                cs_bp_data     = <fs_bp_data>
            ).


            "PARTNER - Central - Address
            me->mt_fill_part_cent_address_cust(
              EXPORTING
                im_object_task = lv_object_task      " Objects data
                is_addr1_data  = gs_addr1_data       " Estrutura de transferência para endereços
                is_sza1_d0100  = gs_sza1_d0100       " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
                is_kna1        = vg_kna1
                it_adsmtp      = gt_adsmtp           " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
              CHANGING
                cs_bp_data     = <fs_bp_data>         " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
            ).

            "PARTNER - Central - Tax
            me->mt_fill_part_cent_tax_cust(
              EXPORTING
                im_object_task = lv_object_task
                is_kna1        = vg_kna1
              CHANGING
                cs_bp_data     = <fs_bp_data>
            ).

            CALL FUNCTION 'BAPI_BUPA_ROLES_GET_2'
              EXPORTING
                businesspartner      = lva_bu_partner   " Business Partner Number
              TABLES
                businesspartnerroles = lt_rules.         " Business Partner Roles

            IF lt_rules IS NOT INITIAL.

              LOOP AT lt_rules ASSIGNING FIELD-SYMBOL(<fs_rules>).

                me->mt_fill_part_cent_roles_sup(
                  EXPORTING
                    im_object_task = lv_object_task                  " Interface externa: código de modificação objeto
                    im_role        = <fs_rules>-partnerrolecategory                  " Função de parceiro de negócios
                  CHANGING
                    cs_bp_data     = <fs_bp_data>                 " Interface complexa do parceiro negócios na integ.clnt.forn.
                ).

              ENDLOOP.
            ENDIF.

            "Supplier - Data
            me->mt_fill_part_cust_data(
              EXPORTING
                is_kna1    = vg_kna1               " Atual.dados mestre fornecedor: campos de tela e operativos
              CHANGING
                cs_bp_data = <fs_bp_data>           " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "Supplier - Address
            me->mt_fill_part_cust_address(
              EXPORTING
                im_object_task = lv_object_task                " Interface externa: código de modificação objeto
                is_kna1        = vg_kna1
                is_addr1_data  = gs_addr1_data                " Estrutura de transferência para endereços
              CHANGING
                cs_bp_data     = <fs_bp_data>                " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            "Supplier - Contact
            me->mt_fill_part_cust_contact(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação objeto
                is_kna1        = vg_kna1
                is_sza1_d0100  = gs_sza1_d0100      " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
                is_knvk        = vg_knvk
                it_adsmtp      = gt_adsmtp          " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            CLEAR: gt_knvi.
            SELECT * FROM knvi
             INTO TABLE gt_knvi
              WHERE kunnr EQ bapi_kunnr.

            me->mt_fill_part_cust_tax(
              EXPORTING
                im_object_task = lv_object_task
                it_knvi        = gt_knvi
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            CLEAR: gt_knb1.
            SELECT * FROM knb1
             INTO TABLE gt_knb1
              WHERE kunnr EQ bapi_kunnr.

            CLEAR: gt_knbw.
            SELECT * FROM knbw
              INTO TABLE gt_knbw
               WHERE kunnr EQ bapi_kunnr.

            me->mt_fill_part_cust_company(
              EXPORTING
                im_object_task = lv_object_task
                it_knb1        = gt_knb1
                it_knbw        = gt_knbw
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            CLEAR: gt_knvv.
            SELECT * FROM knvv
              INTO TABLE gt_knvv
               WHERE kunnr EQ bapi_kunnr.

            me->mt_fill_part_cust_sales(
               EXPORTING
                 im_object_task = lv_object_task
                 it_knvv        = gt_knvv
               CHANGING
                 cs_bp_data     = <fs_bp_data>
             ).

            me->mt_fill_part_cust_bankdetail(
              EXPORTING
                im_object_task = lv_object_task     " Interface externa: código de modificação dados bancários
                it_knbk        = gt_knbk            " Interface externa: dados detalhes bancários
              CHANGING
                cs_bp_data     = <fs_bp_data>       " Interface complexa do parceiro negócios na integ.clnt.forn.
            ).

            UNASSIGN <fs_bp_data>.

            "Valida antes de efetivar alteração.
            DATA(ls_data_up) = lt_bp_data[ 1 ].
            CLEAR: lt_return_map[].
            cl_md_bp_maintain=>validate_single(
              EXPORTING
                i_data        = ls_data_up
              IMPORTING
                et_return_map = lt_return_map ).

            IF lt_return_map[] IS NOT INITIAL.
              LOOP AT lt_return_map ASSIGNING <fs_l_return_map>.
                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return>.
                MOVE-CORRESPONDING <fs_l_return_map> TO <fs_return>.
                REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
                UNASSIGN <fs_return>.
              ENDLOOP.
              LOOP AT et_return INTO DATA(lwa_return_up).
                CONCATENATE 'Cliente não atualizado pelo motivo:!'  lwa_return_up-message
               INTO vg_message SEPARATED BY space.

                wa_tx-id_integracao     = i_id_integracao.
                wa_tx-origem_cadastro   = 'EC'.
                wa_tx-id_origem         = w_data_inbound-idorigem.
                wa_tx-id_erro           = lwa_return_up-number.
                wa_tx-msg_processamento =  vg_message.
                wa_tx-status_proc       = 'E'.
                wa_tx-id_cli_processado = vg_kunnr.
                wa_tx-dt_registro       = sy-datum.
                wa_tx-hr_registro       = sy-uzeit.
                wa_tx-us_registro       = sy-uname.

                MODIFY zsdt0327tx FROM wa_tx.
                CLEAR: wa_tx, vg_message.
                COMMIT WORK.

              ENDLOOP.

              RETURN.
            ENDIF.
*----------------------------------------------------------------------*
*  CRIA BP
*----------------------------------------------------------------------*
            "IMPORTANTE!
            "Se por algum motivo algum dado não está sendo preenchido mesmo sendo passado
            "para a classe, e nenhum erro está retornando, ver transação MDS_PPO2.
            "Foi nessa transação que eu vi que os dados da KNB1 não estavam sendo gravados
            "porque o campo KNA1-CFOPC não estava sendo informado.
            CLEAR: lt_return[].
            CALL METHOD cl_md_bp_maintain=>maintain
              EXPORTING
                i_data     = lt_bp_data
                i_test_run = c_test
              IMPORTING
                e_return   = lt_return.

            LOOP AT lt_return ASSIGNING <fs_l_return>.
              LOOP AT <fs_l_return>-object_msg ASSIGNING <fs_object_msg>.
                APPEND INITIAL LINE TO et_return ASSIGNING <fs_return>.
                MOVE-CORRESPONDING <fs_object_msg> TO <fs_return>.
                REPLACE ALL OCCURRENCES OF 'A' IN <fs_return>-type WITH 'E'.
                UNASSIGN <fs_return>.
              ENDLOOP.
            ENDLOOP.

            IF line_exists( et_return[ type = 'E' ] ).

              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              lva_erro = 'X'.

              LOOP AT et_return INTO lwa_return.

                CONCATENATE 'Cliente não criado pelo motivo:!'   lwa_return-message
                INTO vg_message SEPARATED BY space.

                wa_tx-id_integracao     = i_id_integracao.
                wa_tx-origem_cadastro   = 'EC'.
                wa_tx-id_origem         = w_data_inbound-idorigem.
                wa_tx-id_erro           = lwa_return-number.
                wa_tx-msg_processamento = vg_message.
                wa_tx-status_proc       = 'E'.
                wa_tx-id_cli_processado = vg_kunnr.
                wa_tx-dt_registro       = sy-datum.
                wa_tx-hr_registro       = sy-uzeit.
                wa_tx-us_registro       = sy-uname.

                MODIFY zsdt0327tx FROM wa_tx.
                CLEAR: wa_tx, lwa_return.
                COMMIT WORK.
              ENDLOOP.

            ELSE.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              SELECT SINGLE partner
                FROM but000
              INTO @DATA(lv_partner_up)
                WHERE partner_guid = @ls_data_up-partner-header-object_instance-bpartnerguid.

              SELECT SINGLE * FROM ibupacustomer
                INTO @DATA(wa_ibupacustomer_up)
                WHERE businesspartner = @lv_partner_up
                AND businesspartneruuid = @ls_data_up-partner-header-object_instance-bpartnerguid. .


              CONCATENATE 'Cliente' wa_ibupacustomer_up-customer 'atualizado com sucesso!'
                                    INTO vg_message SEPARATED BY space.

              wa_tx-id_integracao     = i_id_integracao.
              wa_tx-id_origem         =  w_data_inbound-idorigem.
              wa_tx-origem_cadastro   = 'EC'.
              wa_tx-msg_processamento = vg_message.
              wa_tx-status_proc       = 'S'.
              wa_tx-id_cli_processado = bapi_kunnr.
              wa_tx-dt_registro       = sy-datum.
              wa_tx-hr_registro       = sy-uzeit.
              wa_tx-us_registro       = sy-uname.

              MODIFY zsdt0327tx FROM wa_tx.
              CLEAR: wa_tx.
              COMMIT WORK.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: it_zsdt0317[], it_zsdt0319[], it_zsdt0320[], it_zsdt0322[].
      ENDLOOP.
    ELSE.
      wa_tx-id_integracao     = i_id_integracao.
      wa_tx-id_origem         = w_data_inbound-idorigem.
      wa_tx-origem_cadastro   = 'EC'.
      wa_tx-msg_processamento = 'Erro na desserialização JSON'.
      wa_tx-status_proc       = 'E'.
      wa_tx-id_cli_processado = ''.
      wa_tx-dt_registro       = sy-datum.
      wa_tx-hr_registro       = sy-uzeit.
      wa_tx-us_registro       = sy-uname.

      MODIFY zsdt0327tx FROM wa_tx.
      CLEAR: wa_tx.
      COMMIT WORK.
      lva_erro = 'X'.
    ENDIF.

    IF lva_erro IS NOT INITIAL.
      e_sucesso      = abap_false.
    ELSE.
      e_sucesso      = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
