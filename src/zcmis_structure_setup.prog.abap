*&---------------------------------------------------------------------*
*& Report  ZCMIS_STRUCTURE_SETUP_V8
*&---------------------------------------------------------------------*
*& Objetivo: Cria uma estrutura de pastas no Alfresco via REST API.
*&           Esta versão adiciona uma chamada de "aquecimento" (GET)
*&           para estabilizar a conexão HTTP antes das chamadas de
*&           criação (POST), resolvendo o erro 400 no primeiro item.
*& Versão:   8.0 (Versão de Produção)
*& Autor:    Gemini (Revisão Final)
*& Data:     30.09.2025
*&---------------------------------------------------------------------*
REPORT zcmis_structure_setup_v8.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_sap_object,
    name     TYPE string,
    node_type TYPE string,
  END OF ty_sap_object.

*----------------------------------------------------------------------*
* CLASS lcl_alfresco_client DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alfresco_client DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_rfc_destination TYPE rfcdest
          iv_api_base_path   TYPE string
        RAISING
          cx_ai_system_fault,
      warm_up_connection,
      create_node
        IMPORTING
          is_sap_object     TYPE ty_sap_object
          iv_parent_node_id TYPE string,
      close_connection.

  PRIVATE SECTION.
    METHODS:
      escape_json_value
        IMPORTING
          iv_value        TYPE string
        RETURNING
          VALUE(rv_value) TYPE string.
    DATA:
      mo_http_client   TYPE REF TO if_http_client,
      mv_api_base_path TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_alfresco_client IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alfresco_client IMPLEMENTATION.
  METHOD constructor.
    mv_api_base_path = iv_api_base_path.
    CALL METHOD cl_http_client=>create_by_destination
      EXPORTING
        destination = iv_rfc_destination
      IMPORTING
        client      = mo_http_client
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_fault.
    ENDIF.
  ENDMETHOD.

  METHOD warm_up_connection.
    " Este método faz uma chamada de leitura inofensiva para inicializar
    " completamente a sessão HTTP antes de enviar dados (POST).
    mo_http_client->request->set_method( if_http_request=>co_request_method_get ).
    CALL METHOD cl_http_utility=>set_request_uri
      EXPORTING
        request = mo_http_client->request
        uri     = mv_api_base_path.
    TRY.
        mo_http_client->send( ).
        mo_http_client->receive( ).
      CATCH cx_web_http_client_error.
        " Não é necessário tratar o erro aqui, a criação do nó irá falhar
        " e reportar o problema de comunicação de qualquer maneira.
    ENDTRY.
  ENDMETHOD.

  METHOD create_node.
    DATA: lv_url              TYPE string,
          lv_payload          TYPE string,
          lv_response         TYPE string,
          lv_status_code      TYPE i,
          lv_reason           TYPE string,
          lv_sanitized_name   TYPE string,
          lv_escaped_name     TYPE string,
          lv_escaped_nodetype TYPE string.

    lv_sanitized_name = replace( val = is_sap_object-name sub = ':' with = '_' occ = 0 ).
    lv_escaped_name = me->escape_json_value( lv_sanitized_name ).
    lv_escaped_nodetype = me->escape_json_value( is_sap_object-node_type ).

    CONCATENATE '{"name":"'       lv_escaped_name     '","nodeType":"'
                lv_escaped_nodetype '"}'
           INTO lv_payload.

    CONCATENATE mv_api_base_path '/nodes/' iv_parent_node_id '/children' INTO lv_url.

    CALL METHOD cl_http_utility=>set_request_uri
      EXPORTING
        request = mo_http_client->request
        uri     = lv_url.

    mo_http_client->request->set_method( if_http_request=>co_request_method_post ).
    mo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json; charset=utf-8' ).
    mo_http_client->request->set_cdata( data = lv_payload ).

    TRY.
        mo_http_client->send( ).
        mo_http_client->receive( ).
      CATCH cx_web_http_client_error INTO DATA(lo_comm_error).
        WRITE: / 'ERRO DE COMUNICAÇÃO:', |Falha ao processar nó "{ is_sap_object-name }": { lo_comm_error->get_text( ) }|.
        RETURN.
    ENDTRY.

    mo_http_client->response->get_status( IMPORTING code = lv_status_code reason = lv_reason ).
    lv_response = mo_http_client->response->get_cdata( ).

    CASE lv_status_code.
      WHEN 201.
        WRITE: / 'SUCESSO:', |Nó "{ lv_sanitized_name }" ({ is_sap_object-node_type }) criado com sucesso.|.
      WHEN 409.
        WRITE: / 'AVISO:', |Nó "{ lv_sanitized_name }" já existe. Ignorando.|.
      WHEN 401.
        WRITE: / 'ERRO CRÍTICO:', |Falha de autenticação (401). Verifique o destino RFC.|.
      WHEN 403.
        WRITE: / 'ERRO CRÍTICO:', |Acesso negado (403). Verifique as permissões do usuário.|.
      WHEN OTHERS.
        WRITE: / 'ERRO INESPERADO:', |Status { lv_status_code } ({ lv_reason }) ao criar "{ is_sap_object-name }". Resposta: { lv_response }|.
    ENDCASE.
  ENDMETHOD.

  METHOD escape_json_value.
    rv_value = iv_value.
    rv_value = replace( val = rv_value sub = `\` with = `\\` occ = 0 ).
    rv_value = replace( val = rv_value sub = `"` with = `\"` occ = 0 ).
  ENDMETHOD.

  METHOD close_connection.
    IF mo_http_client IS BOUND.
      mo_http_client->close( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* GLOBAL DATA
*----------------------------------------------------------------------*
CONSTANTS:
  gc_rfc_destination TYPE rfcdest VALUE 'ALFRESCO_QAS',
  gc_api_base_path   TYPE string  VALUE '/alfresco/api/-default-/public/alfresco/versions/1'.

DATA:
  gt_sap_objects TYPE TABLE OF ty_sap_object,
  gs_sap_object  TYPE ty_sap_object,
  go_client      TYPE REF TO lcl_alfresco_client.

*----------------------------------------------------------------------*
* MAIN PROGRAM LOGIC
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM main.

*&---------------------------------------------------------------------*
*&      Form  main
*&---------------------------------------------------------------------*
FORM main.
  WRITE: / 'STATUS: Iniciando criação da estrutura no Alfresco.'.

  PERFORM build_sap_object_list.

  TRY.
      CREATE OBJECT go_client
        EXPORTING
          iv_rfc_destination = gc_rfc_destination
          iv_api_base_path   = gc_api_base_path.
      WRITE: / 'SUCESSO: Conexão RFC estabelecida com sucesso.'.

      " AÇÃO: Fazer a chamada de aquecimento antes do loop de criação.
      WRITE: / 'STATUS: Aquecendo a conexão HTTP...'.
      go_client->warm_up_connection( ).

    CATCH cx_ai_system_fault.
      MESSAGE 'Falha ao conectar com o destino RFC. Verifique a configuração na SM59.' TYPE 'E'.
      EXIT.
    CATCH cx_root INTO DATA(lo_error).
      MESSAGE |Erro inesperado na conexão RFC: { lo_error->get_text( ) }| TYPE 'E'.
      EXIT.
  ENDTRY.

  LOOP AT gt_sap_objects INTO gs_sap_object.
    CALL METHOD go_client->create_node
      EXPORTING
        is_sap_object     = gs_sap_object
        iv_parent_node_id = '-root-'.
  ENDLOOP.

  DATA(lt_env_folders) = VALUE string_table( ( `DMS_ALFRESCO_QAS` ) ( `DMS_ALFRESCO_PRD` ) ).
  LOOP AT lt_env_folders INTO DATA(lv_folder).
    CALL METHOD go_client->create_node
      EXPORTING
        is_sap_object     = VALUE #( name = lv_folder node_type = 'cm:folder' )
        iv_parent_node_id = '-root-'.
  ENDLOOP.

  go_client->close_connection( ).
  WRITE: / 'STATUS: Processo de setup finalizado.'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_sap_object_list
*&---------------------------------------------------------------------*
FORM build_sap_object_list.
  gt_sap_objects = VALUE #(
    ( name = 'sapecm:uri'                   node_type = 'cm:content' )
    ( name = 'sapbo:boBase'                 node_type = 'cm:folder' )
    ( name = 'sapbo:basedocument'           node_type = 'cm:content' )
    ( name = 'sapbo:basefolder'             node_type = 'cm:folder' )
    ( name = 'sapbo:ArchiveLink'            node_type = 'cm:content' )
    ( name = 'sapbo:KnowledgeProvider'      node_type = 'cm:folder' )
    ( name = 'sapbo:BusinessObjectType'     node_type = 'cm:folder' )
    ( name = 'sapbo:SAPoffice'              node_type = 'cm:folder' )
    ( name = 'sapbo:draft'                  node_type = 'cm:content' )
    ( name = 'sapbo:NodeBase'               node_type = 'cm:content' )
    ( name = 'sapbo:NodeHeader'             node_type = 'cm:content' )
    ( name = 'sapbo:InternalMigration'      node_type = 'cm:content' )
    ( name = 'sapbo:BankAccount'            node_type = 'cm:content' )
    ( name = 'sapbo:BankPowerOfAttorney'    node_type = 'cm:content' )
    ( name = 'sapbo:BillingDocument'        node_type = 'cm:content' )
    ( name = 'sapbo:BillOfMaterial'         node_type = 'cm:content' )
    ( name = 'sapbo:BudgetDocument'         node_type = 'cm:content' )
    ( name = 'sapbo:BusinessPartner'        node_type = 'cm:content' )
    ( name = 'sapbo:BusinessSituation'      node_type = 'cm:content' )
    ( name = 'sapbo:BusinessSolutionOrder'  node_type = 'cm:content' )
    ( name = 'sapbo:BusinessSolutionQuotation' node_type = 'cm:content' )
    ( name = 'sapbo:CentralPurchaseContract' node_type = 'cm:content' )
    ( name = 'sapbo:CentralPurchaseRequisition' node_type = 'cm:content' )
    ( name = 'sapbo:CentralRequestForQuotation' node_type = 'cm:content' )
    ( name = 'sapbo:CentralSupplierQuotation' node_type = 'cm:content' )
    ( name = 'sapbo:ChangeMaster'           node_type = 'cm:content' )
    ( name = 'sapbo:ChangeRecord'           node_type = 'cm:content' )
    ( name = 'sapbo:CollectionsEmail'       node_type = 'cm:content' )
    ( name = 'sapbo:ConditionContract'      node_type = 'cm:content' )
    ( name = 'sapbo:ContrAcctgBillingDocument' node_type = 'cm:content' )
    ( name = 'sapbo:ContrAcctgBillingPlan'  node_type = 'cm:content' )
    ( name = 'sapbo:ContrAcctgBillingRequest' node_type = 'cm:content' )
    ( name = 'sapbo:ContrAcctgInvcgClrfctnCase' node_type = 'cm:content' )
    ( name = 'sapbo:ContrAcctgInvoicingDocument' node_type = 'cm:content' )
    ( name = 'sapbo:CostCenterActivityType' node_type = 'cm:content' )
    ( name = 'sapbo:CreditManagementAccount' node_type = 'cm:content' )
    ( name = 'sapbo:CreditMemoRequest'      node_type = 'cm:content' )
    ( name = 'sapbo:CreditMgmtBusinessPartner' node_type = 'cm:content' )
    ( name = 'sapbo:Customer'               node_type = 'cm:content' )
    ( name = 'sapbo:CustomerCompanyCode'    node_type = 'cm:content' )
    ( name = 'sapbo:CustomerContactPerson'  node_type = 'cm:content' )
    ( name = 'sapbo:CustomerReturn'         node_type = 'cm:content' )
    ( name = 'sapbo:CustomerSalesArea'      node_type = 'cm:content' )
    ( name = 'sapbo:CustomerSettlement'     node_type = 'cm:content' )
    ( name = 'sapbo:CustomerSettlementList' node_type = 'cm:content' )
    ( name = 'sapbo:DebitMemoRequest'       node_type = 'cm:content' )
    ( name = 'sapbo:Defect'                 node_type = 'cm:content' )
    ( name = 'sapbo:DocumentInfoRecord'     node_type = 'cm:content' )
    ( name = 'sapbo:EnterpriseProject'      node_type = 'cm:folder' )
    ( name = 'sapbo:EnterpriseProjectRanking' node_type = 'cm:content' )
    ( name = 'sapbo:Equipment'              node_type = 'cm:content' )
    ( name = 'sapbo:FldLogsShptContainer'   node_type = 'cm:folder' )
    ( name = 'sapbo:FldLogsShptCtnCertificate' node_type = 'cm:content' )
    ( name = 'sapbo:FunctionalLocation'     node_type = 'cm:content' )
    ( name = 'sapbo:HardwareConstraint'     node_type = 'cm:content' )
    ( name = 'sapbo:InboundDelivery'        node_type = 'cm:content' )
    ( name = 'sapbo:InspectionLot'          node_type = 'cm:content' )
    ( name = 'sapbo:InspectionMethod'       node_type = 'cm:content' )
    ( name = 'sapbo:InspectionSpecification' node_type = 'cm:content' )
    ( name = 'sapbo:JournalEntry'           node_type = 'cm:content' )
    ( name = 'sapbo:KanbanControlCycle'     node_type = 'cm:content' )
    ( name = 'sapbo:LegalCategory'          node_type = 'cm:content' )
    ( name = 'sapbo:LegalContext'           node_type = 'cm:content' )
    ( name = 'sapbo:LegalDocument'          node_type = 'cm:content' )
    ( name = 'sapbo:LegalTransaction'       node_type = 'cm:content' )
    ( name = 'sapbo:MaintenanceNotification' node_type = 'cm:content' )
    ( name = 'sapbo:MaintenanceOrder'       node_type = 'cm:content' )
    ( name = 'sapbo:MaintenancePlan'        node_type = 'cm:content' )
    ( name = 'sapbo:MaintenanceTaskList'    node_type = 'cm:content' )
    ( name = 'sapbo:MaintenanceTaskListOperation' node_type = 'cm:content' )
    ( name = 'sapbo:MasterWarranty'         node_type = 'cm:content' )
    ( name = 'sapbo:OutboundDelivery'       node_type = 'cm:content' )
    ( name = 'sapbo:OutputRequest'          node_type = 'cm:content' )
    ( name = 'sapbo:PaymentRequest'         node_type = 'cm:content' )
    ( name = 'sapbo:PersonnelSettlementDocument' node_type = 'cm:content' )
    ( name = 'sapbo:PlannedOrder'           node_type = 'cm:content' )
    ( name = 'sapbo:PRAAccountReceivable'   node_type = 'cm:content' )
    ( name = 'sapbo:PRADivnOfInterestLease' node_type = 'cm:content' )
    ( name = 'sapbo:PRAPriorPeriodNotification' node_type = 'cm:content' )
    ( name = 'sapbo:PRARevenueAccountingDocument' node_type = 'cm:content' )
    ( name = 'sapbo:PRATaxesPayable'        node_type = 'cm:content' )
    ( name = 'sapbo:PreliminaryBillingDocument' node_type = 'cm:content' )
    ( name = 'sapbo:ProblemSolvingProcess'  node_type = 'cm:content' )
    ( name = 'sapbo:ProcessOrder'           node_type = 'cm:content' )
    ( name = 'sapbo:Product'                node_type = 'cm:content' )
    ( name = 'sapbo:ProductAllocationObject' node_type = 'cm:content' )
    ( name = 'sapbo:ProductAllocationSequence' node_type = 'cm:content' )
    ( name = 'sapbo:ProductionOrder'        node_type = 'cm:content' )
    ( name = 'sapbo:ProductStructure'       node_type = 'cm:content' )
    ( name = 'sapbo:PurchaseContract'       node_type = 'cm:content' )
    ( name = 'sapbo:PurchaseContractItem'   node_type = 'cm:content' )
    ( name = 'sapbo:PurchaseOrder'          node_type = 'cm:content' )
    ( name = 'sapbo:PurchaseRequisition'    node_type = 'cm:content' )
    ( name = 'sapbo:PurchaseSchedulingAgreement' node_type = 'cm:content' )
    ( name = 'sapbo:PurchasingCategory'     node_type = 'cm:content' )
    ( name = 'sapbo:PurchasingInfoRecord'   node_type = 'cm:content' )
    ( name = 'sapbo:PurchasingQualityInfoRecord' node_type = 'cm:content' )
    ( name = 'sapbo:PurchasingQuotaArrangement' node_type = 'cm:content' )
    ( name = 'sapbo:QltyProcurementCertificate' node_type = 'cm:content' )
    ( name = 'sapbo:Recipe'                 node_type = 'cm:content' )
    ( name = 'sapbo:RecipeLabelSet'         node_type = 'cm:content' )
    ( name = 'sapbo:RecipeRawSubstance'     node_type = 'cm:content' )
    ( name = 'sapbo:RecipeRealSubstance'    node_type = 'cm:content' )
    ( name = 'sapbo:RequestForQuotation'    node_type = 'cm:content' )
    ( name = 'sapbo:RevenueAccountingContract' node_type = 'cm:content' )
    ( name = 'sapbo:SalesContract'          node_type = 'cm:content' )
    ( name = 'sapbo:SalesInquiry'           node_type = 'cm:content' )
    ( name = 'sapbo:SalesOrder'             node_type = 'cm:content' )
    ( name = 'sapbo:SalesQuotation'         node_type = 'cm:content' )
    ( name = 'sapbo:SalesSchedulingAgreement' node_type = 'cm:content' )
    ( name = 'sapbo:ServiceConfirmation'    node_type = 'cm:content' )
    ( name = 'sapbo:ServiceContract'        node_type = 'cm:content' )
    ( name = 'sapbo:ServiceEntrySheet'      node_type = 'cm:content' )
    ( name = 'sapbo:ServiceOrder'           node_type = 'cm:content' )
    ( name = 'sapbo:SettlementDocument'     node_type = 'cm:content' )
    ( name = 'sapbo:SoftwareConstraint'     node_type = 'cm:content' )
    ( name = 'sapbo:SourcingProject'        node_type = 'cm:folder' )
    ( name = 'sapbo:Supplier'               node_type = 'cm:content' )
    ( name = 'sapbo:SupplierCompanyCode'    node_type = 'cm:content' )
    ( name = 'sapbo:SupplierInvoice'        node_type = 'cm:content' )
    ( name = 'sapbo:SupplierInvoiceUpload'  node_type = 'cm:content' )
    ( name = 'sapbo:SupplierPurchasingOrganization' node_type = 'cm:content' )
    ( name = 'sapbo:SupplierQuotation'      node_type = 'cm:content' )
    ( name = 'sapbo:WorkflowDemoChangeRequest' node_type = 'cm:content' )
    ( name = 'sapbo:WorkflowDemoLeaveRequest' node_type = 'cm:content' )
    ( name = 'sapbo:WorkforcePerson'        node_type = 'cm:content' )
  ).
ENDFORM.
