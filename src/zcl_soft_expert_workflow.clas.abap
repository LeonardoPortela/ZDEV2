class ZCL_SOFT_EXPERT_WORKFLOW definition
  public
  create public .

public section.

  interfaces ZIF_SOFT_EXPERT_WORKFLOW .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SOFT_EXPERT_WORKFLOW IMPLEMENTATION.


  METHOD zif_soft_expert_workflow~cancel_workflow.

    DATA: ob_web_service TYPE REF TO zcl_webservice,
          lc_xml         TYPE string,
          i_name_file    TYPE string.

    r_instance = me.

    CLEAR: E_CANCEL_WORFLOW_RET.

    LC_XML = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
    CONCATENATE LC_XML '<soapenv:Header/>' INTO LC_XML.
    CONCATENATE LC_XML '<soapenv:Body>' INTO LC_XML.

    CONCATENATE LC_XML '<urn:cancelWorkflow>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:WorkflowID>' I_WORKFLOWID '</urn:WorkflowID>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:Explanation>' I_EXPLANATION '</urn:Explanation>' INTO LC_XML.
    CONCATENATE LC_XML '</urn:cancelWorkflow>' INTO LC_XML.

    CONCATENATE LC_XML '</soapenv:Body>' INTO LC_XML.
    CONCATENATE LC_XML '</soapenv:Envelope>' INTO LC_XML.

    CREATE OBJECT OB_WEB_SERVICE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'SE' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = '1' ).

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO IS NOT INITIAL.
        OB_WEB_SERVICE->SET_USUARIO( I_USUARIO = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO ) ).
      ENDIF.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA IS NOT INITIAL.
        OB_WEB_SERVICE->SET_SENHA( I_SENHA = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA ) ).
      ENDIF.
    ENDIF.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP I_AUTENTICAR = ABAP_TRUE ).

    CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = LC_XML
        I_NOT_CONTENT_LENGTH       = ABAP_TRUE
      RECEIVING
        E_RESULTADO                = DATA(XML_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CLEAR: OB_WEB_SERVICE.

    DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0007)
      FROM ZLEST0007
     WHERE ID_CTG       = 'XML_SE'
       AND PREFIX       = 'XML'.

    IF SY-SUBRC IS INITIAL.
      CREATE OBJECT ARQUIVO.
      CONCATENATE WA_ZLEST0007-PATHUNIX 'CancelWorkFlow' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER LC_XML TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CONCATENATE WA_ZLEST0007-PATHUNIX 'CancelWorkFlowOut' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER XML_RETORNO TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CLEAR: ARQUIVO.
    ENDIF.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~CANCEL_WORKFLOW_READ_RETORN( EXPORTING I_XML = XML_RETORNO I_URI = CONV #( LC_URI ) IMPORTING E_CANCEL_WORKFLOW = E_CANCEL_WORFLOW_RET ).

    IF E_CANCEL_WORFLOW_RET-STATUS NE 'SUCCESS'.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_CANCEL_WORFLOW_RET-DETAIL ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_soft_expert_workflow~cancel_workflow_novo.

    DATA: ob_web_service TYPE REF TO zcl_webservice,
          lc_xml         TYPE string,
          i_name_file    TYPE string.

    DATA: newworkflow     TYPE zde_se_new_worflow,
          lc_new_workflow TYPE zde_se_new_worflow.

    r_instance = me.

    me->zif_soft_expert_workflow~get_processid( IMPORTING e_id_processid = newworkflow-processid
     )->get_workflowtitle( IMPORTING e_workflowtitle = newworkflow-workflowtitle
     )->get_userid( IMPORTING e_userid = newworkflow-userid
     ).

    lc_new_workflow-processid     = newworkflow-processid.
    lc_new_workflow-workflowtitle = newworkflow-workflowtitle.
    lc_new_workflow-userid        = newworkflow-userid.

    TRY .

        e_new_workflow = i_new_workflow.

        "Edita Atributos
        me->zif_soft_expert_workflow~edit_attributes( EXPORTING i_workflowid = e_new_workflow-recordid
        "Edita Entitys
         )->edit_editentitys( EXPORTING i_workflowid = e_new_workflow-recordid
        "Anexar Arquivos na Atividade do WorkFlow
         )->new_attachments( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
        "Executa as Atividades Iniciais do WorkFlow
         )->set_exec_activitys( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
                                          i_atividade = 'CANCELAR'
         ).


      CATCH zcx_soft_expert_workflow INTO DATA(zxe_soft_expert_workflow).

        "Chegou a Gerar o WorkFlow (Deve ser Cancelado)
        IF e_new_workflow-status EQ 'SUCCESS' AND i_new_workflow-recordid IS INITIAL.

          MESSAGE ID zxe_soft_expert_workflow->msgid TYPE 'S'
           NUMBER zxe_soft_expert_workflow->msgno WITH zxe_soft_expert_workflow->msgv1 zxe_soft_expert_workflow->msgv2 zxe_soft_expert_workflow->msgv3 zxe_soft_expert_workflow->msgv4
             INTO DATA(i_texto).

        ENDIF.

        RAISE EXCEPTION TYPE zcx_soft_expert_workflow
          EXPORTING
            textid    = VALUE #( msgid = zxe_soft_expert_workflow->msgid
                                 msgno = zxe_soft_expert_workflow->msgno
                                 attr1 = CONV #( zxe_soft_expert_workflow->msgv1 )
                                 attr2 = CONV #( zxe_soft_expert_workflow->msgv2 )
                                 attr3 = CONV #( zxe_soft_expert_workflow->msgv3 )
                                 attr4 = CONV #( zxe_soft_expert_workflow->msgv4 )
               )
            msgty     = zxe_soft_expert_workflow->msgty
            msgno     = zxe_soft_expert_workflow->msgno
            msgv1     = zxe_soft_expert_workflow->msgv1
            msgv2     = zxe_soft_expert_workflow->msgv2
            msgv3     = zxe_soft_expert_workflow->msgv3
            msgv4     = zxe_soft_expert_workflow->msgv4
            msgid     = zxe_soft_expert_workflow->msgid
            transacao = zxe_soft_expert_workflow->transacao.
    ENDTRY.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~CANCEL_WORKFLOW_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CLEAR: E_CANCEL_WORKFLOW.

    CREATE OBJECT LC_XML_RET.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa cancelWorkflowResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'cancelWorkflowResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'cancelWorkflowResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'cancelWorkflowResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'cancelWorkflowResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'cancelWorkflowResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_CANCEL_WORKFLOW-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'cancelWorkflowResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'cancelWorkflowResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_CANCEL_WORKFLOW-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'cancelWorkflowResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'cancelWorkflowResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_CANCEL_WORKFLOW-DETAIL = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~CLEAR.

    R_INSTANCE = ME.

    CLEAR: ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ATRIBUTES[],
           ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS[],
           ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS_ATRIBUTES[],
           ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ACTIVITYS[],
           ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_WORKFLOW.

  ENDMETHOD.


  METHOD zif_soft_expert_workflow~create_new_workflow.

    DATA: newworkflow     TYPE zde_se_new_worflow,
          lc_new_workflow TYPE zde_se_new_worflow.

    r_instance = me.

    me->zif_soft_expert_workflow~get_processid( IMPORTING e_id_processid = newworkflow-processid
     )->get_workflowtitle( IMPORTING e_workflowtitle = newworkflow-workflowtitle
     )->get_userid( IMPORTING e_userid = newworkflow-userid
     ).

    lc_new_workflow-processid     = newworkflow-processid.
    lc_new_workflow-workflowtitle = newworkflow-workflowtitle.
    lc_new_workflow-userid        = newworkflow-userid.

    "Se já criou uma SM sem startar as atividades inicias do Worlflow, não prosseguir...
    IF i_desativar IS NOT INITIAL AND ( i_new_workflow-recordid IS NOT INITIAL ).
      e_new_workflow = i_new_workflow.
      EXIT.
    ENDIF.

    TRY .

        IF i_new_workflow-recordid IS INITIAL.

          IF i_desativar EQ abap_true.

            "Cria WorkFlow
            me->zif_soft_expert_workflow~new_workflow(
               EXPORTING i_new_workflow = lc_new_workflow
               IMPORTING e_new_workflow = e_new_workflow ).
*            "Edita Atributos
*             )->edit_attributes( EXPORTING i_workflowid = e_new_workflow-recordid
*            "Edita Entitys
*             )->edit_editentitys( EXPORTING i_workflowid = e_new_workflow-recordid
*            "Anexar Arquivos na Atividade do WorkFlow
*             )->new_attachments( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
*            "Executa as Atividades Iniciais do WorkFlow
*             ")->SET_EXEC_ACTIVITYS( EXPORTING I_NEW_WORKFLOW = LC_NEW_WORKFLOW I_WORKFLOWID = E_NEW_WORKFLOW-RECORDID
*             ).

          ELSE.

            "Cria WorkFlow
            me->zif_soft_expert_workflow~new_workflow(
               EXPORTING i_new_workflow = lc_new_workflow
               IMPORTING e_new_workflow = e_new_workflow
            "Edita Atributos
             )->edit_attributes( EXPORTING i_workflowid = e_new_workflow-recordid
            "Edita Entitys
             )->edit_editentitys( EXPORTING i_workflowid = e_new_workflow-recordid
            "Anexar Arquivos na Atividade do WorkFlow
             )->new_attachments( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
            "Executa as Atividades Iniciais do WorkFlow
             )->set_exec_activitys( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
             ).

          ENDIF.


        ELSE.

          e_new_workflow = i_new_workflow.

          "Edita Atributos
          me->zif_soft_expert_workflow~edit_attributes( EXPORTING i_workflowid = e_new_workflow-recordid
          "Edita Entitys
           )->edit_editentitys( EXPORTING i_workflowid = e_new_workflow-recordid
          "Anexar Arquivos na Atividade do WorkFlow
           )->new_attachments( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
          "Executa as Atividades Iniciais do WorkFlow
           )->set_exec_activitys( EXPORTING i_new_workflow = lc_new_workflow i_workflowid = e_new_workflow-recordid
           ).

        ENDIF.

      CATCH zcx_soft_expert_workflow INTO DATA(zxe_soft_expert_workflow).

        "Chegou a Gerar o WorkFlow (Deve ser Cancelado)
        IF e_new_workflow-status EQ 'SUCCESS' AND i_new_workflow-recordid IS INITIAL.
          MESSAGE ID zxe_soft_expert_workflow->msgid TYPE 'S'
           NUMBER zxe_soft_expert_workflow->msgno WITH zxe_soft_expert_workflow->msgv1 zxe_soft_expert_workflow->msgv2 zxe_soft_expert_workflow->msgv3 zxe_soft_expert_workflow->msgv4
             INTO DATA(i_texto).

*          ME->ZIF_SOFT_EXPERT_WORKFLOW~CANCEL_WORKFLOW(
*             EXPORTING
*               I_WORKFLOWID  = E_NEW_WORKFLOW-RECORDID
*               I_EXPLANATION = I_TEXTO
*             IMPORTING
*                E_CANCEL_WORFLOW_RET = DATA(E_CANCEL_WORFLOW_RET) ).
        ENDIF.

        RAISE EXCEPTION TYPE zcx_soft_expert_workflow
          EXPORTING
            textid    = VALUE #( msgid = zxe_soft_expert_workflow->msgid
                                 msgno = zxe_soft_expert_workflow->msgno
                                 attr1 = CONV #( zxe_soft_expert_workflow->msgv1 )
                                 attr2 = CONV #( zxe_soft_expert_workflow->msgv2 )
                                 attr3 = CONV #( zxe_soft_expert_workflow->msgv3 )
                                 attr4 = CONV #( zxe_soft_expert_workflow->msgv4 )
               )
            msgty     = zxe_soft_expert_workflow->msgty
            msgno     = zxe_soft_expert_workflow->msgno
            msgv1     = zxe_soft_expert_workflow->msgv1
            msgv2     = zxe_soft_expert_workflow->msgv2
            msgv3     = zxe_soft_expert_workflow->msgv3
            msgv4     = zxe_soft_expert_workflow->msgv4
            msgid     = zxe_soft_expert_workflow->msgid
            transacao = zxe_soft_expert_workflow->transacao.
    ENDTRY.



  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ATTRIBUTES.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE,
          LC_XML         TYPE STRING,
          I_NAME_FILE    TYPE STRING.

    R_INSTANCE = ME.

    CLEAR: E_EDIT_ATTRIBUTES_RETORNO.

    LC_XML = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
    CONCATENATE LC_XML '<soapenv:Header/>' INTO LC_XML.
    CONCATENATE LC_XML '<soapenv:Body>' INTO LC_XML.

    CONCATENATE LC_XML '<urn:editAttributeValue>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:WorkflowID>' I_WORKFLOWID '</urn:WorkflowID>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:AttributeList>' INTO LC_XML.

    IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_ATTRIBUTES_VALUES(
             EXPORTING I_WORKFLOW = ME
             IMPORTING E_XML_ATRIBUTOS = DATA(E_XML_ATRIBUTOS) ).
    ELSE.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_ATTRIBUTES_VALUES(
        IMPORTING
          E_XML_ATRIBUTOS = E_XML_ATRIBUTOS ).
    ENDIF.

    CHECK E_XML_ATRIBUTOS IS NOT INITIAL.
    CONCATENATE LC_XML E_XML_ATRIBUTOS INTO LC_XML.
    CONCATENATE LC_XML '</urn:AttributeList>' INTO LC_XML.
    CONCATENATE LC_XML '</urn:editAttributeValue>' INTO LC_XML.

    CONCATENATE LC_XML '</soapenv:Body>' INTO LC_XML.
    CONCATENATE LC_XML '</soapenv:Envelope>' INTO LC_XML.

    CREATE OBJECT OB_WEB_SERVICE.

    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'SE' ).
      CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
    ENDTRY.

    OB_WEB_SERVICE->SET_TIPO( I_TIPO = '1' ).

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO IS NOT INITIAL.
        OB_WEB_SERVICE->SET_USUARIO( I_USUARIO = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO ) ).
      ENDIF.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA IS NOT INITIAL.
        OB_WEB_SERVICE->SET_SENHA( I_SENHA = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA ) ).
      ENDIF.
    ENDIF.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP I_AUTENTICAR = ABAP_TRUE ).

    CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = LC_XML
        I_NOT_CONTENT_LENGTH       = ABAP_TRUE
      RECEIVING
        E_RESULTADO                = DATA(XML_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    CLEAR: OB_WEB_SERVICE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0007)
      FROM ZLEST0007
     WHERE ID_CTG       = 'XML_SE'
       AND PREFIX       = 'XML'.

    IF SY-SUBRC IS INITIAL.
      CREATE OBJECT ARQUIVO.
      CONCATENATE WA_ZLEST0007-PATHUNIX 'EditAttibutesWorkFlow' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER LC_XML TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CONCATENATE WA_ZLEST0007-PATHUNIX 'EditAttibutesWorkFlowOut' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER XML_RETORNO TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CLEAR: ARQUIVO.
    ENDIF.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ATTRIBUTES_READ_RETORN( EXPORTING I_XML = XML_RETORNO I_URI = CONV #( LC_URI ) IMPORTING E_EDIT_ATTRIBUTES_RETORNO = E_EDIT_ATTRIBUTES_RETORNO ).

    IF E_EDIT_ATTRIBUTES_RETORNO-STATUS NE 'SUCCESS'.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_EDIT_ATTRIBUTES_RETORNO-DETAIL ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ATTRIBUTES_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CLEAR: E_EDIT_ATTRIBUTES_RETORNO.

    CREATE OBJECT LC_XML_RET.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa editAttributeValueResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'editAttributeValueResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editAttributeValueResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editAttributeValueResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editAttributeValueResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editAttributeValueResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ATTRIBUTES_RETORNO-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editAttributeValueResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editAttributeValueResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ATTRIBUTES_RETORNO-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editAttributeValueResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editAttributeValueResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ATTRIBUTES_RETORNO-DETAIL = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ATTRIBUTES_VALUE_XML.

    DATA: LC_VALOR_10 TYPE C LENGTH 08,
          LC_VALOR_HR TYPE C LENGTH 06.

    R_INSTANCE = ME.

    CHECK I_VALOR IS NOT INITIAL.

    READ TABLE ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ATRIBUTES INTO DATA(WA_ATRIBUTES_X_SAP)
                                                         WITH KEY TABNAME   = I_TABELA
                                                                  FIELDNAME = I_CAMPO.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE E_XML '<urn:Attribute>' INTO E_XML.
      CONCATENATE E_XML '<urn:AttributeID>' WA_ATRIBUTES_X_SAP-ATTRIBUTEID '</urn:AttributeID>' INTO E_XML.
      CONCATENATE E_XML '<urn:AttributeValueList>' INTO E_XML.
      CONCATENATE E_XML '<urn:AttributeValue>' INTO E_XML.
      CASE WA_ATRIBUTES_X_SAP-TYPE.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO.
          CONCATENATE E_XML I_VALOR INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA.
          MOVE I_VALOR TO LC_VALOR_10.
          CONCATENATE LC_VALOR_10+6(2) '/' LC_VALOR_10+4(2) '/' LC_VALOR_10(4) INTO DATA(LC_DATA).
          CONCATENATE E_XML LC_DATA INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_HORA.
          MOVE I_VALOR TO LC_VALOR_HR.
          CONCATENATE LC_VALOR_HR(2) ':' LC_VALOR_HR+2(2) ':' LC_VALOR_HR+4(2) INTO DATA(LC_HORA).
          CONCATENATE E_XML LC_HORA INTO E_XML.
      ENDCASE.
      CONCATENATE E_XML '</urn:AttributeValue>' INTO E_XML.

      CONCATENATE E_XML '</urn:AttributeValueList>' INTO E_XML.
      CONCATENATE E_XML '</urn:Attribute>' INTO E_XML.
    ELSE.
      "eRRO
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_EDITENTITYS.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE,
          LC_XML         TYPE STRING,
          LC_XML_CHILD   TYPE STRING,
          I_NAME_FILE    TYPE STRING.

    R_INSTANCE = ME.

    LOOP AT ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS INTO DATA(WA_ENTITYS) WHERE ENTITYID_PARENT IS INITIAL.
      "Edit Entity

      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
        ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_MANY_EDITENTITYS_RECORDS(
           EXPORTING I_WORKFLOW = ME I_ENTITYID = CONV #( WA_ENTITYS-ENTITYID )
           IMPORTING E_MANY =  DATA(E_MANY) ).
      ELSE.
        ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_MANY_EDITENTITYS_RECORDS( EXPORTING I_ENTITYID = CONV #( WA_ENTITYS-ENTITYID ) IMPORTING E_MANY = E_MANY ).
      ENDIF.

      "Cria Entidade Main
      DO E_MANY TIMES.

        CLEAR: E_EDIT_ENTITY_RETORNO.

        LC_XML = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
        CONCATENATE LC_XML '<soapenv:Header/>' INTO LC_XML.
        CONCATENATE LC_XML '<soapenv:Body>' INTO LC_XML.

        CONCATENATE LC_XML '<urn:editEntityRecord>' INTO LC_XML.
        CONCATENATE LC_XML '<urn:WorkflowID>' I_WORKFLOWID '</urn:WorkflowID>' INTO LC_XML.
        CONCATENATE LC_XML '<urn:EntityID>' WA_ENTITYS-ENTITYID '</urn:EntityID>' INTO LC_XML.

        CONCATENATE LC_XML '<urn:EntityAttributeList>' INTO LC_XML.

        IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.

          ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_ENTITYS_VALUES(
             EXPORTING
               I_WORKFLOW = ME
               I_ENTITYID = CONV #( WA_ENTITYS-ENTITYID )
               I_INDEX    = SY-INDEX
             IMPORTING
               E_XML_ENTITY = DATA(E_XML_ENTITY)
               E_RELATIONSHIP	= DATA(E_RELATIONSHIP) ).
        ELSE.
          ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS_VALUES(
             EXPORTING
               I_ENTITYID = CONV #( WA_ENTITYS-ENTITYID )
               I_INDEX    = SY-INDEX
             IMPORTING
               E_XML_ENTITY = E_XML_ENTITY
               E_RELATIONSHIP	= E_RELATIONSHIP ).
        ENDIF.
        IF E_XML_ENTITY IS INITIAL.
          CONTINUE.
        ENDIF.
        CONCATENATE LC_XML E_XML_ENTITY INTO LC_XML.
        CONCATENATE LC_XML '</urn:EntityAttributeList>' INTO LC_XML.

        CONCATENATE LC_XML '<urn:RelationshipList>' INTO LC_XML.

        IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
          ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_RELATIONSHIPS_VALUES(
             EXPORTING
               I_WORKFLOW = ME
               I_ENTITYID = CONV #( WA_ENTITYS-ENTITYID )
               I_INDEX    = SY-INDEX
             IMPORTING
               E_RELATIONSHIP	= DATA(E_XML_RELATIONSHIPLIST) ).
        ELSE.
          ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_RELATIONSHIPS_VALUES(
            EXPORTING
              I_ENTITYID     = CONV #( WA_ENTITYS-ENTITYID )
              I_INDEX        = SY-INDEX
            IMPORTING
              E_RELATIONSHIP = E_XML_RELATIONSHIPLIST ).
        ENDIF.

        CONCATENATE LC_XML E_XML_RELATIONSHIPLIST INTO LC_XML.
        CONCATENATE LC_XML '</urn:RelationshipList>' INTO LC_XML.

        CONCATENATE LC_XML '</urn:editEntityRecord>' INTO LC_XML.

        CONCATENATE LC_XML '</soapenv:Body>' INTO LC_XML.
        CONCATENATE LC_XML '</soapenv:Envelope>' INTO LC_XML.

        CREATE OBJECT OB_WEB_SERVICE.

        TRY .
            OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'SE' ).
          CATCH ZCX_WEBSERVICE INTO DATA(LC_EXCEPTION).
        ENDTRY.

        OB_WEB_SERVICE->SET_TIPO( I_TIPO = '1' ).

        TRY .
            DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
            DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
          CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        ENDTRY.

        IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
          IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO IS NOT INITIAL.
            OB_WEB_SERVICE->SET_USUARIO( I_USUARIO = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO ) ).
          ENDIF.
          IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA IS NOT INITIAL.
            OB_WEB_SERVICE->SET_SENHA( I_SENHA = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA ) ).
          ENDIF.
        ENDIF.

        OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP  I_AUTENTICAR = ABAP_TRUE ).

        CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
          EXPORTING
            I_HTTP                     = VAR_HTTP
            I_XML                      = LC_XML
            I_NOT_CONTENT_LENGTH       = ABAP_TRUE
          RECEIVING
            E_RESULTADO                = DATA(XML_RETORNO)
          EXCEPTIONS
            HTTP_COMMUNICATION_FAILURE = 1
            HTTP_INVALID_STATE         = 2
            HTTP_PROCESSING_FAILED     = 3
            HTTP_INVALID_TIMEOUT       = 4
            OTHERS                     = 5.

        IF SY-SUBRC IS NOT INITIAL.
          RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
            EXPORTING
              TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
              MSGID  = SY-MSGID
              MSGNO  = SY-MSGNO
              MSGTY  = 'E'
              MSGV1  = SY-MSGV1
              MSGV2  = SY-MSGV2
              MSGV3  = SY-MSGV3
              MSGV4  = SY-MSGV4.
        ENDIF.

        CLEAR: OB_WEB_SERVICE.

        DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

        SELECT SINGLE * INTO @DATA(WA_ZLEST0007)
          FROM ZLEST0007
         WHERE ID_CTG       = 'XML_SE'
           AND PREFIX       = 'XML'.

        IF SY-SUBRC IS INITIAL.
          CREATE OBJECT ARQUIVO.
          CONCATENATE WA_ZLEST0007-PATHUNIX 'editEntityRecord' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

          OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
          IF SY-SUBRC IS INITIAL.
            TRANSFER LC_XML TO I_NAME_FILE.
            CLOSE DATASET I_NAME_FILE.
          ENDIF.

          CONCATENATE WA_ZLEST0007-PATHUNIX 'editEntityRecordOut' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

          OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
          IF SY-SUBRC IS INITIAL.
            TRANSFER XML_RETORNO TO I_NAME_FILE.
            CLOSE DATASET I_NAME_FILE.
          ENDIF.

          CLEAR: ARQUIVO.
        ENDIF.

        ME->ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITYS_READ_RETORN( EXPORTING I_XML = XML_RETORNO I_URI = CONV #( LC_URI ) IMPORTING E_EDIT_ENTITY_RETORNO = E_EDIT_ENTITY_RETORNO ).

        IF E_EDIT_ENTITY_RETORNO-STATUS NE 'SUCCESS'.
          ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_EDIT_ENTITY_RETORNO-DETAIL ).
        ENDIF.

        LOOP AT ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS INTO DATA(WA_ENTITYS_CHILD)
           WHERE ENTITYID_PARENT EQ WA_ENTITYS-ENTITYID.

          IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
            ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_MANY_EDITENTITYS_RECORDS(
                EXPORTING I_WORKFLOW = ME
                          I_ENTITYID = CONV #( WA_ENTITYS_CHILD-ENTITYID )
                          I_RELATIONSHIP = E_RELATIONSHIP
                IMPORTING E_MANY =  DATA(E_MANY_CHILD) ).
          ELSE.
            ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_MANY_EDITENTITYS_RECORDS(
                EXPORTING I_ENTITYID = CONV #( WA_ENTITYS_CHILD-ENTITYID )
                          I_RELATIONSHIP = E_RELATIONSHIP
                IMPORTING E_MANY = E_MANY_CHILD ).
          ENDIF.

          DO E_MANY TIMES.

            LC_XML_CHILD = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
            CONCATENATE LC_XML_CHILD '<soapenv:Header/>' INTO LC_XML_CHILD.
            CONCATENATE LC_XML_CHILD '<soapenv:Body>' INTO LC_XML_CHILD.

            CONCATENATE LC_XML_CHILD '<urn:newChildEntityRecord>' INTO LC_XML_CHILD.
            CONCATENATE LC_XML_CHILD '<urn:WorkflowID>' I_WORKFLOWID '</urn:WorkflowID>' INTO LC_XML_CHILD.
            CONCATENATE LC_XML_CHILD '<urn:MainEntityID>' WA_ENTITYS-ENTITYID '</urn:MainEntityID>' INTO LC_XML_CHILD.
            CONCATENATE LC_XML_CHILD '<urn:ChildRelationshipID>' WA_ENTITYS_CHILD-RELATIONSHIP_ID '</urn:ChildRelationshipID>' INTO LC_XML_CHILD.

            CONCATENATE LC_XML_CHILD '<urn:EntityAttributeList>' INTO LC_XML_CHILD.

            IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
              ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_ENTITYS_CHILD_VALUES(
                 EXPORTING
                   I_WORKFLOW = ME
                   I_ENTITYID = CONV #( WA_ENTITYS_CHILD-ENTITYID )
                   I_INDEX    = SY-INDEX
                   I_RELATIONSHIP = E_RELATIONSHIP
                 IMPORTING
                   E_XML_ENTITY_CHILD = DATA(E_XML_ENTITY_CHILD) ).
            ELSE.
              ME->ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS_CHILD_VALUES(
                EXPORTING
                  I_ENTITYID     = CONV #( WA_ENTITYS_CHILD-ENTITYID )
                  I_INDEX        = SY-INDEX
                  I_RELATIONSHIP = E_RELATIONSHIP
                IMPORTING
                 E_XML_ENTITY_CHILD = E_XML_ENTITY_CHILD ).
            ENDIF.

            IF E_XML_ENTITY_CHILD IS INITIAL.
              CONTINUE.
            ENDIF.

            CONCATENATE LC_XML_CHILD E_XML_ENTITY_CHILD INTO LC_XML_CHILD.

            CONCATENATE LC_XML_CHILD '</urn:EntityAttributeList>' INTO LC_XML_CHILD.

            CONCATENATE LC_XML_CHILD '</urn:newChildEntityRecord>' INTO LC_XML_CHILD.

            CONCATENATE LC_XML_CHILD '</soapenv:Body>' INTO LC_XML_CHILD.
            CONCATENATE LC_XML_CHILD '</soapenv:Envelope>' INTO LC_XML_CHILD.

            CREATE OBJECT OB_WEB_SERVICE.

            TRY .
                OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'SE' ).
              CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
            ENDTRY.

            OB_WEB_SERVICE->SET_TIPO( I_TIPO = '1' ).

            TRY .
                VAR_HTTP = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
                LC_URI = OB_WEB_SERVICE->GET_URI(  ).
              CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
            ENDTRY.

            OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP  I_AUTENTICAR = ABAP_TRUE ).

            CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
              EXPORTING
                I_HTTP                     = VAR_HTTP
                I_XML                      = LC_XML_CHILD
                I_NOT_CONTENT_LENGTH       = ABAP_TRUE
              RECEIVING
                E_RESULTADO                = XML_RETORNO
              EXCEPTIONS
                HTTP_COMMUNICATION_FAILURE = 1
                HTTP_INVALID_STATE         = 2
                HTTP_PROCESSING_FAILED     = 3
                HTTP_INVALID_TIMEOUT       = 4
                OTHERS                     = 5.

            IF SY-SUBRC IS NOT INITIAL.
              RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
                EXPORTING
                  TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
                  MSGID  = SY-MSGID
                  MSGNO  = SY-MSGNO
                  MSGTY  = 'E'
                  MSGV1  = SY-MSGV1
                  MSGV2  = SY-MSGV2
                  MSGV3  = SY-MSGV3
                  MSGV4  = SY-MSGV4.
            ENDIF.

            CLEAR: OB_WEB_SERVICE.

            SELECT SINGLE * INTO WA_ZLEST0007
              FROM ZLEST0007
             WHERE ID_CTG       = 'XML_SE'
               AND PREFIX       = 'XML'.

            IF SY-SUBRC IS INITIAL.
              CREATE OBJECT ARQUIVO.
              CONCATENATE WA_ZLEST0007-PATHUNIX 'newChildEntityRecord' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

              OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
              IF SY-SUBRC IS INITIAL.
                TRANSFER LC_XML_CHILD TO I_NAME_FILE.
                CLOSE DATASET I_NAME_FILE.
              ENDIF.

              CONCATENATE WA_ZLEST0007-PATHUNIX 'newChildEntityRecordOut' I_WORKFLOWID '.xml' INTO I_NAME_FILE.

              OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
              IF SY-SUBRC IS INITIAL.
                TRANSFER XML_RETORNO TO I_NAME_FILE.
                CLOSE DATASET I_NAME_FILE.
              ENDIF.

              CLEAR: ARQUIVO.
            ENDIF.

            ME->ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITYS_NEWCHILD_VALUE_RT(
              EXPORTING I_XML = XML_RETORNO I_URI = CONV #( LC_URI )
              IMPORTING E_NEW_CHILD_RETORNO = DATA(E_NEW_CHILD_RETORNO) ).

            IF E_NEW_CHILD_RETORNO-STATUS NE 'SUCCESS'.
              ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_NEW_CHILD_RETORNO-DETAIL ).
            ENDIF.
          ENDDO.
        ENDLOOP.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITYS_NEWCHILD_VALUE_RT.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CREATE OBJECT LC_XML_RET.

    CLEAR: E_NEW_CHILD_RETORNO.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa editEntityRecordResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'newChildEntityRecordResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newChildEntityRecordResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newChildEntityRecordResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newChildEntityRecordResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newChildEntityRecordResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_CHILD_RETORNO-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newChildEntityRecordResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newChildEntityRecordResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_CHILD_RETORNO-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newChildEntityRecordResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newChildEntityRecordResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_CHILD_RETORNO-DETAIL = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'RecordKey'
      RECEIVING
        NODE = LC_XML_NODE.

    IF ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      E_NEW_CHILD_RETORNO-RECORDKEY = LC_XML_NODE->GET_VALUE( ).
    ENDIF.

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITYS_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CREATE OBJECT LC_XML_RET.

    CLEAR: E_EDIT_ENTITY_RETORNO.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa editEntityRecordResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'editEntityRecordResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editEntityRecordResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editEntityRecordResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editEntityRecordResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editEntityRecordResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ENTITY_RETORNO-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editEntityRecordResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editEntityRecordResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ENTITY_RETORNO-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'editEntityRecordResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'editEntityRecordResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EDIT_ENTITY_RETORNO-DETAIL = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITY_CHILD_VALUE_XML.

    DATA: LC_VALOR_10 TYPE C LENGTH 08,
          LC_VALOR_HR TYPE C LENGTH 06.

    R_INSTANCE = ME.

    CHECK I_VALOR IS NOT INITIAL.

    READ TABLE ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS_ATRIBUTES INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP)
          WITH KEY ENTITYID = I_ENTITYID
                   TABNAME  = I_TABELA
                   FIELDNAME = I_CAMPO.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE E_XML '<urn:RelationshipAttribute>' INTO E_XML.
      CONCATENATE E_XML '<urn:RelationshipAttributeID>' WA_ENTITYS_ATRIBUTES_X_SAP-ATTRIBUTEID '</urn:RelationshipAttributeID>' INTO E_XML.

      CONCATENATE E_XML '<urn:RelationshipAttributeValue>' INTO E_XML.
      CASE WA_ENTITYS_ATRIBUTES_X_SAP-TYPE.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO.
          CONCATENATE E_XML I_VALOR INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA.
          MOVE I_VALOR TO LC_VALOR_10.
          CONCATENATE LC_VALOR_10+6(2) '/' LC_VALOR_10+4(2) '/' LC_VALOR_10(4) INTO DATA(LC_DATA).
          CONCATENATE E_XML LC_DATA INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_HORA.
          MOVE I_VALOR TO LC_VALOR_HR.
          CONCATENATE LC_VALOR_HR(2) ':' LC_VALOR_HR+2(2) ':' LC_VALOR_HR+4(2) INTO DATA(LC_HORA).
          CONCATENATE E_XML LC_HORA INTO E_XML.
      ENDCASE.
      CONCATENATE E_XML '</urn:RelationshipAttributeValue>' INTO E_XML.

      CONCATENATE E_XML '</urn:RelationshipAttribute>' INTO E_XML.
    ELSE.
      "eRRO
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITY_NEWCHILD_VALUE_XML.

    DATA: LC_VALOR_10 TYPE C LENGTH 08,
          LC_VALOR_HR TYPE C LENGTH 06.

    R_INSTANCE = ME.

    CHECK I_VALOR IS NOT INITIAL.

    READ TABLE ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS_ATRIBUTES INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP)
          WITH KEY ENTITYID = I_ENTITYID
                   TABNAME  = I_TABELA
                   FIELDNAME = I_CAMPO.

    IF SY-SUBRC IS INITIAL.
      CONCATENATE E_XML '<urn:EntityAttribute>' INTO E_XML.
      CONCATENATE E_XML '<urn:EntityAttributeID>' WA_ENTITYS_ATRIBUTES_X_SAP-ATTRIBUTEID '</urn:EntityAttributeID>' INTO E_XML.

      CONCATENATE E_XML '<urn:EntityAttributeValue>' INTO E_XML.
      CASE WA_ENTITYS_ATRIBUTES_X_SAP-TYPE.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO.
          CONCATENATE E_XML I_VALOR INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA.
          MOVE I_VALOR TO LC_VALOR_10.
          CONCATENATE LC_VALOR_10+6(2) '/' LC_VALOR_10+4(2) '/' LC_VALOR_10(4) INTO DATA(LC_DATA).
          CONCATENATE E_XML LC_DATA INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_HORA.
          MOVE I_VALOR TO LC_VALOR_HR.
          CONCATENATE LC_VALOR_HR(2) ':' LC_VALOR_HR+2(2) ':' LC_VALOR_HR+4(2) INTO DATA(LC_HORA).
          CONCATENATE E_XML LC_HORA INTO E_XML.
      ENDCASE.
      CONCATENATE E_XML '</urn:EntityAttributeValue>' INTO E_XML.

      CONCATENATE E_XML '</urn:EntityAttribute>' INTO E_XML.
    ELSE.
      "eRRO
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~EDIT_ENTITY_VALUE_XML.

    DATA: LC_VALOR_10 TYPE C LENGTH 08,
          LC_VALOR_HR TYPE C LENGTH 06.

    DATA: I_OUTTEXT TYPE STRING.

*    DATA: SPECHARSEAR(50) VALUE ''' _ < > ! " & / = ? : ; , . # # % ^ $ | ~ '.
    DATA: SPECHARSEAR(50) VALUE ''' _ < > ! " & / = ? : ; , # # % ^ $ | ~ '.

    R_INSTANCE = ME.

    "CHECK I_VALOR IS NOT INITIAL.

    I_OUTTEXT = I_VALOR.
    TRANSLATE I_OUTTEXT USING SPECHARSEAR.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        INTEXT            = I_OUTTEXT
      IMPORTING
        OUTTEXT           = I_OUTTEXT
      EXCEPTIONS
        INVALID_CODEPAGE  = 1
        CODEPAGE_MISMATCH = 2
        INTERNAL_ERROR    = 3
        CANNOT_CONVERT    = 4
        FIELDS_NOT_TYPE_C = 5
        OTHERS            = 6.

    IF I_ATTRIBUTEID IS INITIAL.

      READ TABLE ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS_ATRIBUTES INTO DATA(WA_ENTITYS_ATRIBUTES_X_SAP)
            WITH KEY ENTITYID = I_ENTITYID
                     TABNAME = I_TABELA
                     FIELDNAME = I_CAMPO.

      IF SY-SUBRC IS INITIAL.
        CONCATENATE E_XML '<urn:EntityAttribute>' INTO E_XML.
        CONCATENATE E_XML '<urn:EntityAttributeID>' WA_ENTITYS_ATRIBUTES_X_SAP-ATTRIBUTEID '</urn:EntityAttributeID>' INTO E_XML.

        CONCATENATE E_XML '<urn:EntityAttributeValue>' INTO E_XML.
        CASE WA_ENTITYS_ATRIBUTES_X_SAP-TYPE.
          WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO.
            CONCATENATE E_XML I_OUTTEXT INTO E_XML.
          WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO.
            CONCATENATE E_XML I_VALOR INTO E_XML.
          WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA.
            CONCATENATE E_XML I_OUTTEXT INTO E_XML.
          WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA.
            MOVE I_OUTTEXT TO LC_VALOR_10.
            CONCATENATE LC_VALOR_10(4) '-' LC_VALOR_10+4(2) '-' LC_VALOR_10+6(2) INTO DATA(LC_DATA).
            CONCATENATE E_XML LC_DATA INTO E_XML.
          WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_HORA.
            MOVE I_OUTTEXT TO LC_VALOR_HR.
            CONCATENATE LC_VALOR_HR(2) ':' LC_VALOR_HR+2(2) INTO DATA(LC_HORA).
            CONCATENATE E_XML LC_HORA INTO E_XML.
        ENDCASE.
        CONCATENATE E_XML '</urn:EntityAttributeValue>' INTO E_XML.

        CONCATENATE E_XML '</urn:EntityAttribute>' INTO E_XML.
      ELSE.
        "eRRO
      ENDIF.
    ELSE.
      CONCATENATE E_XML '<urn:EntityAttribute>' INTO E_XML.
      CONCATENATE E_XML '<urn:EntityAttributeID>' I_ATTRIBUTEID '</urn:EntityAttributeID>' INTO E_XML.
      CONCATENATE E_XML '<urn:EntityAttributeValue>' INTO E_XML.
      CASE I_TYPE.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_TEXTO.
          CONCATENATE E_XML I_OUTTEXT INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_NUMERICO.
          CONCATENATE E_XML I_VALOR INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_MOEDA.
          CONCATENATE E_XML I_OUTTEXT INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_DATA.
          MOVE I_OUTTEXT TO LC_VALOR_10.
          CONCATENATE LC_VALOR_10(4) '-' LC_VALOR_10+4(2) '-' LC_VALOR_10+6(2) INTO LC_DATA.
          CONCATENATE E_XML LC_DATA INTO E_XML.
        WHEN ZIF_SOFT_EXPERT_WORKFLOW=>ST_TYPE_FIELD_VALUE_HORA.
          MOVE I_OUTTEXT TO LC_VALOR_HR.
          CONCATENATE LC_VALOR_HR(2) ':' LC_VALOR_HR+2(2) INTO LC_HORA.
          CONCATENATE E_XML LC_HORA INTO E_XML.
        WHEN OTHERS.
          CONCATENATE E_XML I_OUTTEXT INTO E_XML.
      ENDCASE.
      CONCATENATE E_XML '</urn:EntityAttributeValue>' INTO E_XML.
      CONCATENATE E_XML '</urn:EntityAttribute>' INTO E_XML.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ACTIVITYS.

    R_INSTANCE = ME.

    SELECT * INTO TABLE E_ACTIVITYS
      FROM ZSEXPT00005
     WHERE PROCESSID EQ I_ID_PROCESSID
     ORDER BY SEQUENCIA_EXEC.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ATRIBUTES.

    R_INSTANCE = ME.

    SELECT * INTO TABLE E_ATRIBUTES
      FROM ZSEXPT00002
     WHERE PROCESSID EQ I_ID_PROCESSID.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ATTRIBUTES_VALUES.

    R_INSTANCE = ME.

    CLEAR: E_XML_ATRIBUTOS.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS.

    R_INSTANCE = ME.

    SELECT * INTO TABLE E_ENTITYS
      FROM ZSEXPT00003
     WHERE PROCESSID EQ I_ID_PROCESSID.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS_ATTRIBUTES.

    R_INSTANCE = ME.

    SELECT * INTO TABLE E_ENTITYS_ATTRIBUTES
      FROM ZSEXPT00004
     WHERE PROCESSID EQ I_ID_PROCESSID.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS_CHILD_VALUES.

    R_INSTANCE = ME.
    CLEAR: E_XML_ENTITY_CHILD.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_ENTITYS_VALUES.

    R_INSTANCE = ME.
    CLEAR: E_XML_ENTITY.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_INSTANCE.

    IF ZIF_SOFT_EXPERT_WORKFLOW~AT_SOFT_EXPERT IS NOT BOUND.
      CREATE OBJECT ZIF_SOFT_EXPERT_WORKFLOW~AT_SOFT_EXPERT TYPE ZCL_SOFT_EXPERT_WORKFLOW.
    ENDIF.

    R_INSTANCE = ZIF_SOFT_EXPERT_WORKFLOW~AT_SOFT_EXPERT.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_MANY_EDITENTITYS_RECORDS.

    R_INSTANCE = ME.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_PROCESSID.
    R_INSTANCE = ME.
    E_ID_PROCESSID = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_WORKFLOW-PROCESSID.
  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_PROCESSID_SE.

    DATA: LC_TP_AMBIENTE TYPE ZDE_TP_AMBIENTE.

*01	Desenvolvimento
*02	Homologação
*03	Produção

    CASE SY-SYSID.
      WHEN 'DEV'.
        LC_TP_AMBIENTE = '01'.
      WHEN 'QAS'.
        LC_TP_AMBIENTE = '02'.
      WHEN 'PRD'.
        LC_TP_AMBIENTE = '03'.
    ENDCASE.

    SELECT SINGLE * INTO @E_ZSEXPT00006
      FROM ZSEXPT00006
     WHERE TP_AMBIENTE   EQ @LC_TP_AMBIENTE
       AND PROCESSID_SAP EQ @I_PROCESSID_SAP.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID    = VALUE #( MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_PROCESS_SAPSE-MSGID
                               MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_PROCESS_SAPSE-MSGNO
                               ATTR1  = CONV #( I_PROCESSID_SAP )
                               ATTR2  = CONV #( LC_TP_AMBIENTE ) )
          MSGID     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_PROCESS_SAPSE-MSGID
          MSGNO     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_PROCESS_SAPSE-MSGNO
          MSGV1     = CONV #( I_PROCESSID_SAP )
          MSGV2     = CONV #( LC_TP_AMBIENTE )
          MSGTY     = 'E'
          TRANSACAO = 'ZSE00006'.
    ELSE.
      R_PROCESSID = E_ZSEXPT00006-PROCESSID.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_RELATIONSHIPS_VALUES.

    R_INSTANCE = ME.
    CLEAR: E_RELATIONSHIP.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_USERID.

    R_INSTANCE = ME.

    SELECT SINGLE * INTO @DATA(WA_USR21)
      FROM USR21
     WHERE BNAME EQ @I_USUARIO.

    SELECT SINGLE * INTO @DATA(WA_ADCP)
      FROM ADCP
     WHERE PERSNUMBER EQ @WA_USR21-PERSNUMBER.

    E_USERID = WA_ADCP-FAX_NUMBER.

    CHECK E_USERID IS INITIAL.

    RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_CPF_NAO_ENCONTRADO-MSGID
                          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_CPF_NAO_ENCONTRADO-MSGNO )
        MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_CPF_NAO_ENCONTRADO-MSGID
        MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_CPF_NAO_ENCONTRADO-MSGNO
        MSGTY  = 'E'
        MSGV1  = CONV #( I_USUARIO ).

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_WORKFLOW.

    R_INSTANCE = ME.

    SELECT SINGLE * INTO E_WORKFLOW
      FROM ZSEXPT00001
     WHERE PROCESSID EQ I_ID_PROCESSID.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
      EXPORTING
        TEXTID    = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_WORKFLOW_NAO_ENCONTRADO-MSGID
                             MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_WORKFLOW_NAO_ENCONTRADO-MSGNO )
        MSGID     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_WORKFLOW_NAO_ENCONTRADO-MSGID
        MSGNO     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_WORKFLOW_NAO_ENCONTRADO-MSGNO
        MSGTY     = 'E'
        MSGV1     = CONV #( I_ID_PROCESSID )
        TRANSACAO = 'ZSE0001'.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~GET_WORKFLOWTITLE.

    R_INSTANCE = ME.
    IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->GET_WORKFLOW_TITLE( IMPORTING E_WORKFLOW_TITLE = DATA(E_WORKFLOW_TITLE) ).
      IF E_WORKFLOW_TITLE IS INITIAL.
        E_WORKFLOW_TITLE = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_WORKFLOW-WORKFLOWTITLE.
      ENDIF.
      E_WORKFLOWTITLE = E_WORKFLOW_TITLE.
    ELSE.
      E_WORKFLOWTITLE = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_WORKFLOW-WORKFLOWTITLE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_soft_expert_workflow~new_attachments.

    DATA: ob_web_service TYPE REF TO zcl_webservice,
          arquivo        TYPE REF TO zcl_arquivo,
          lc_xml         TYPE string,
          lc_xml_child   TYPE string,
          i_name_file    TYPE string,
          var_string     TYPE string,
          lv_string      TYPE string,
          lv_xstring     TYPE xstring,
          vl_ini         TYPE i.

    DATA: lt_texto  TYPE TABLE OF char80.

    r_instance = me.


    LOOP AT me->zif_soft_expert_workflow~at_attachment INTO DATA(wa_attachment).

      CLEAR: e_new_attachments_retorno.

      lc_xml = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
      CONCATENATE lc_xml '<soapenv:Header/>' INTO lc_xml.
      CONCATENATE lc_xml '<soapenv:Body>' INTO lc_xml.

      CONCATENATE lc_xml '<urn:newAttachment>' INTO lc_xml.
      CONCATENATE lc_xml '<urn:WorkflowID>' i_workflowid '</urn:WorkflowID>' INTO lc_xml.
      CONCATENATE lc_xml '<urn:ActivityID>' wa_attachment-activityid '</urn:ActivityID>' INTO lc_xml.
      CONCATENATE lc_xml '<urn:FileName>' wa_attachment-filename '</urn:FileName>' INTO lc_xml.

      IF wa_attachment-file_url IS NOT INITIAL AND wa_attachment-file_content IS INITIAL.
        TRY .

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
            vl_ini = strlen( wa_attachment-filename ) - 4.
            IF wa_attachment-filename+vl_ini(4) EQ '.pdf'.
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554

              CREATE OBJECT arquivo.
              arquivo->get_file_uri_get(
                 EXPORTING
                   i_uri = wa_attachment-file_url i_content_type = 'application/pdf'
                 IMPORTING
                   e_texto   = DATA(e_texto)
                   e_texto_2 = DATA(e_texto_2) ).

              CALL FUNCTION 'SSFC_BASE64_ENCODE'
                EXPORTING
                  bindata                  = e_texto_2
*                 BINLENG                  =
                IMPORTING
                  b64data                  = e_texto
                EXCEPTIONS
                  ssf_krn_error            = 1
                  ssf_krn_noop             = 2
                  ssf_krn_nomemory         = 3
                  ssf_krn_opinv            = 4
                  ssf_krn_input_data_error = 5
                  ssf_krn_invalid_par      = 6
                  ssf_krn_invalid_parlen   = 7
                  OTHERS                   = 8.

              CONCATENATE lc_xml '<urn:FileContent>' e_texto '</urn:FileContent>' INTO lc_xml.

*Inicio Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
            ELSEIF wa_attachment-filename+vl_ini(4) EQ '.url'.
              CONSTANTS:
                c_crlf(2) TYPE c VALUE 'XX'.

              "Dados principal.
              CONCATENATE '[{000214A0-0000-0000-C000-000000000046}]'
                          c_crlf 'Prop3=19,11' c_crlf '[InternetShortcut]'
                          c_crlf 'IDList=' c_crlf 'URL=' wa_attachment-file_url
                INTO var_string.

              REPLACE ALL OCCURRENCES OF 'XX' IN var_string WITH cl_abap_char_utilities=>cr_lf.
              "Converter string em XSTRING.
              CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
                EXPORTING
                  text   = var_string
                IMPORTING
                  buffer = lv_xstring
                EXCEPTIONS
                  failed = 1
                  OTHERS = 2.
              IF sy-subrc EQ 0.
                "Converter XSTRING em BASE64.
                CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
                  EXPORTING
                    input  = lv_xstring
                  IMPORTING
                    output = lv_string.
                IF lv_string IS NOT INITIAL.
                  "Dados convertido BASE64 com sucesso.
                ENDIF.
              ENDIF.
              CONCATENATE lc_xml '<urn:FileContent>' lv_string '</urn:FileContent>' INTO lc_xml.
            ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 21.06.23 - #114554
            CLEAR: arquivo.
          CATCH zcx_arquivo INTO DATA(ex_arquivo).
            ex_arquivo->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
            RAISE EXCEPTION TYPE zcx_soft_expert_workflow
              EXPORTING
                textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
                msgid  = sy-msgid
                msgno  = sy-msgno
                msgty  = 'E'
                msgv1  = sy-msgv1
                msgv2  = sy-msgv2
                msgv3  = sy-msgv3
                msgv4  = sy-msgv4.
        ENDTRY.
      ELSE.
        CONCATENATE lc_xml '<urn:FileContent>' wa_attachment-file_content '</urn:FileContent>' INTO lc_xml.
      ENDIF.

      DATA: lc_tp_ambiente TYPE zde_tp_ambiente.

      CASE sy-sysid.
        WHEN 'DEV'.
          lc_tp_ambiente = '01'.
        WHEN 'QAS'.
          lc_tp_ambiente = '02'.
        WHEN 'PRD'.
          lc_tp_ambiente = '03'.
      ENDCASE.

      IF me->zif_soft_expert_workflow~at_object_inject IS BOUND.
        me->zif_soft_expert_workflow~at_workflow-processid = me->zif_soft_expert_workflow~at_object_inject->i_id_processid.
      ENDIF.

      SELECT SINGLE * INTO @DATA(wa_zsexpt00007)
        FROM zsexpt00007
       WHERE processid   EQ @me->zif_soft_expert_workflow~at_workflow-processid
         AND activityid  EQ @wa_attachment-activityid
         AND tp_ambiente EQ @lc_tp_ambiente.

      IF sy-subrc IS INITIAL.
        CONCATENATE lc_xml '<urn:UserID>' wa_zsexpt00007-usuario '</urn:UserID>' INTO lc_xml.
      ELSE.
        CONCATENATE lc_xml '<urn:UserID>' i_new_workflow-userid '</urn:UserID>' INTO lc_xml.
      ENDIF.

      CONCATENATE lc_xml '</urn:newAttachment>' INTO lc_xml.

      CONCATENATE lc_xml '</soapenv:Body>' INTO lc_xml.
      CONCATENATE lc_xml '</soapenv:Envelope>' INTO lc_xml.

      CREATE OBJECT ob_web_service.

      TRY .
          ob_web_service->set_servico( i_servico = 'SE' ).
        CATCH zcx_webservice INTO DATA(lc_exception).
      ENDTRY.

      ob_web_service->set_tipo( i_tipo = '1' ).

      TRY .
          DATA(var_http) = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
          DATA(lc_uri) = ob_web_service->get_uri(  ).
        CATCH zcx_webservice INTO lc_exception.
      ENDTRY.

      IF me->zif_soft_expert_workflow~at_object_inject IS BOUND.
        IF me->zif_soft_expert_workflow~at_object_inject->i_usuario IS NOT INITIAL.
          ob_web_service->set_usuario( i_usuario = CONV #( me->zif_soft_expert_workflow~at_object_inject->i_usuario ) ).
        ENDIF.
        IF me->zif_soft_expert_workflow~at_object_inject->i_senha IS NOT INITIAL.
          ob_web_service->set_senha( i_senha = CONV #( me->zif_soft_expert_workflow~at_object_inject->i_senha ) ).
        ENDIF.
      ENDIF.

      ob_web_service->zif_webservice~abrir_conexao( i_http = var_http  i_autenticar = abap_true ).

      CALL METHOD ob_web_service->zif_webservice~consultar
        EXPORTING
          i_http                     = var_http
          i_xml                      = lc_xml
          i_not_content_length       = abap_true
        RECEIVING
          e_resultado                = DATA(xml_retorno)
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          http_invalid_timeout       = 4
          OTHERS                     = 5.

      IF sy-subrc IS NOT INITIAL.
        RAISE EXCEPTION TYPE zcx_soft_expert_workflow
          EXPORTING
            textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
            msgid  = sy-msgid
            msgno  = sy-msgno
            msgty  = 'E'
            msgv1  = sy-msgv1
            msgv2  = sy-msgv2
            msgv3  = sy-msgv3
            msgv4  = sy-msgv4.
      ENDIF.

      CLEAR: ob_web_service.

      TRY .
          SELECT SINGLE * INTO @DATA(wa_zlest0007)
            FROM zlest0007
           WHERE id_ctg       = 'XML_SE'
             AND prefix       = 'XML'.

          IF sy-subrc IS INITIAL.
            CREATE OBJECT arquivo.
            CONCATENATE wa_zlest0007-pathunix 'newAttachment' i_workflowid wa_attachment-filename '.xml' INTO i_name_file.

            OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
            IF sy-subrc IS INITIAL.
              TRANSFER lc_xml TO i_name_file.
              CLOSE DATASET i_name_file.
            ENDIF.

            CONCATENATE wa_zlest0007-pathunix 'newAttachmentOut' i_workflowid wa_attachment-filename '.xml' INTO i_name_file.

            OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
            IF sy-subrc IS INITIAL.
              TRANSFER xml_retorno TO i_name_file.
              CLOSE DATASET i_name_file.
            ENDIF.
            CLEAR: arquivo.
          ENDIF.
        CATCH cx_root.
      ENDTRY.

      me->zif_soft_expert_workflow~new_attachments_read_retorn( EXPORTING i_xml = xml_retorno i_uri = CONV #( lc_uri ) IMPORTING e_new_attachment = e_new_attachments_retorno ).

      IF e_new_attachments_retorno-status NE 'SUCCESS'.
        me->zif_soft_expert_workflow~gera_erro_geral( EXPORTING i_texto = e_new_attachments_retorno-detail ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~NEW_ATTACHMENTS_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CREATE OBJECT LC_XML_RET.

    CLEAR: E_NEW_ATTACHMENT.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa newWorkflowResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'newAttachmentResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_ATTACHMENT-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_ATTACHMENT-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_ATTACHMENT-DETAIL = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'RecordKey'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).

      IF E_NEW_ATTACHMENT-DETAIL IS NOT INITIAL.
        ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_NEW_ATTACHMENT-DETAIL ).
      ENDIF.

      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse->RecordKey'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse->RecordKey'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_ATTACHMENT-RECORDKEY = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'RecordID'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newAttachmentResponse->RecordID'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newAttachmentResponse->RecordID'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_ATTACHMENT-RECORDID = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~NEW_WORKFLOW.

    DATA: OB_WEB_SERVICE TYPE REF TO ZCL_WEBSERVICE,
          LC_XML         TYPE STRING,
          I_NAME_FILE    TYPE STRING,
          LC_EXCEPTION   TYPE REF TO ZCX_WEBSERVICE.

    R_INSTANCE = ME.


*    DATA(LC_WORKFLOWTITLE) = ZCL_STRING=>TIRA_ACENTOS( I_TEXTO = CONV #( I_NEW_WORKFLOW-WORKFLOWTITLE ) ).

*      CATCH ZCX_ERROR.    "

    CLEAR: E_NEW_WORKFLOW.
    LC_XML = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
    CONCATENATE LC_XML '<soapenv:Header/>' INTO LC_XML.
    CONCATENATE LC_XML '<soapenv:Body>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:newWorkflow>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:ProcessID>'  I_NEW_WORKFLOW-PROCESSID '</urn:ProcessID>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:WorkflowTitle>' I_NEW_WORKFLOW-WORKFLOWTITLE '</urn:WorkflowTitle>' INTO LC_XML.
    CONCATENATE LC_XML '<urn:UserID>' I_NEW_WORKFLOW-USERID '</urn:UserID>' INTO LC_XML.
    CONCATENATE LC_XML '</urn:newWorkflow>' INTO LC_XML.
    CONCATENATE LC_XML '</soapenv:Body>' INTO LC_XML.
    CONCATENATE LC_XML '</soapenv:Envelope>' INTO LC_XML.

    CREATE OBJECT OB_WEB_SERVICE.
    TRY .
        OB_WEB_SERVICE->SET_SERVICO( I_SERVICO = 'SE' ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
          EXPORTING
            TEXTID    = VALUE #( MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_SERVICO_WEBSERVICE-MSGID
                                 MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_SERVICO_WEBSERVICE-MSGNO )
            MSGID     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_SERVICO_WEBSERVICE-MSGID
            MSGNO     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_SERVICO_WEBSERVICE-MSGNO
            MSGTY     = 'E'
            TRANSACAO = 'ZLES0096'.
    ENDTRY.

    TRY .
        OB_WEB_SERVICE->SET_TIPO( I_TIPO = '1' ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
        RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
          EXPORTING
            TEXTID    = VALUE #( MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_TIP_SERVICO_WEBSERVICE-MSGID
                                 MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_TIP_SERVICO_WEBSERVICE-MSGNO )
            MSGID     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_TIP_SERVICO_WEBSERVICE-MSGID
            MSGNO     = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_TIP_SERVICO_WEBSERVICE-MSGNO
            MSGTY     = 'E'
            TRANSACAO = 'ZLES0096'.
    ENDTRY.

    TRY .
        DATA(VAR_HTTP) = OB_WEB_SERVICE->URL( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
        DATA(LC_URI) = OB_WEB_SERVICE->GET_URI(  ).
      CATCH ZCX_WEBSERVICE INTO LC_EXCEPTION.
    ENDTRY.

    IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT IS BOUND.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO IS NOT INITIAL.
        OB_WEB_SERVICE->SET_USUARIO( I_USUARIO = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_USUARIO ) ).
      ENDIF.
      IF ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA IS NOT INITIAL.
        OB_WEB_SERVICE->SET_SENHA( I_SENHA = CONV #( ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT->I_SENHA ) ).
      ENDIF.
    ENDIF.

    OB_WEB_SERVICE->ZIF_WEBSERVICE~ABRIR_CONEXAO( I_HTTP = VAR_HTTP I_AUTENTICAR = ABAP_TRUE ).

    CALL METHOD OB_WEB_SERVICE->ZIF_WEBSERVICE~CONSULTAR
      EXPORTING
        I_HTTP                     = VAR_HTTP
        I_XML                      = LC_XML
        I_NOT_CONTENT_LENGTH       = ABAP_TRUE
      RECEIVING
        E_RESULTADO                = DATA(XML_RETORNO)
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5.

    CLEAR: OB_WEB_SERVICE.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID MSGNO = SY-MSGNO ATTR1 = CONV #( SY-MSGV1 ) ATTR2 = CONV #( SY-MSGV2 ) ATTR3 = CONV #( SY-MSGV3 ) ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    DATA: ARQUIVO TYPE REF TO ZCL_ARQUIVO.

    SELECT SINGLE * INTO @DATA(WA_ZLEST0007)
      FROM ZLEST0007
     WHERE ID_CTG EQ 'XML_SE'
       AND PREFIX EQ 'XML'.

    IF SY-SUBRC IS INITIAL.
      CREATE OBJECT ARQUIVO.
      CONCATENATE WA_ZLEST0007-PATHUNIX 'CreateWorkFlow' I_NEW_WORKFLOW-PROCESSID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER LC_XML TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CONCATENATE WA_ZLEST0007-PATHUNIX 'CreateWorkFlowOut' I_NEW_WORKFLOW-PROCESSID '.xml' INTO I_NAME_FILE.

      OPEN DATASET I_NAME_FILE FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
      IF SY-SUBRC IS INITIAL.
        TRANSFER XML_RETORNO TO I_NAME_FILE.
        CLOSE DATASET I_NAME_FILE.
      ENDIF.

      CLEAR: ARQUIVO.
    ENDIF.

    IF XML_RETORNO IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_EMPTY_RETURN_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_EMPTY_RETURN_WEBSERVICE-MSGNO
                            ATTR1 = CONV #( LC_URI ) )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_EMPTY_RETURN_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_EMPTY_RETURN_WEBSERVICE-MSGNO
          MSGV1  = CONV #( LC_URI )
          MSGTY  = 'E'.
    ENDIF.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~NEW_WORKFLOW_READ_RETORN( EXPORTING I_XML = XML_RETORNO I_URI = CONV #( LC_URI ) IMPORTING E_NEW_WORKFLOW = E_NEW_WORKFLOW ).

    IF E_NEW_WORKFLOW-STATUS NE 'SUCCESS'.
      ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_NEW_WORKFLOW-DETAIL ).
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~NEW_WORKFLOW_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CREATE OBJECT LC_XML_RET.

    CLEAR: E_NEW_WORKFLOW.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa newWorkflowResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'newWorkflowResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_WORKFLOW-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_WORKFLOW-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_WORKFLOW-DETAIL = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'RecordKey'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).

      IF E_NEW_WORKFLOW-DETAIL IS NOT INITIAL.
        ME->ZIF_SOFT_EXPERT_WORKFLOW~GERA_ERRO_GERAL( EXPORTING I_TEXTO = E_NEW_WORKFLOW-DETAIL ).
      ENDIF.

      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse->RecordKey'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse->RecordKey'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_WORKFLOW-RECORDKEY = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'RecordID'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'newWorkflowResponse->RecordID'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'newWorkflowResponse->RecordID'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_NEW_WORKFLOW-RECORDID = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~SET_ATTACHMENT.

    R_INSTANCE = ME.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ATTACHMENT = I_ATTACHMENTS.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~SET_ATTIBUTES_WORKFLOW.

    R_INSTANCE = ME.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~CLEAR(
    )->GET_WORKFLOW(
          EXPORTING I_ID_PROCESSID = I_ID_PROCESSID
          IMPORTING E_WORKFLOW  = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_WORKFLOW
    )->GET_ATRIBUTES(
          EXPORTING I_ID_PROCESSID = I_ID_PROCESSID
          IMPORTING E_ATRIBUTES = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ATRIBUTES
    )->GET_ENTITYS(
          EXPORTING I_ID_PROCESSID = I_ID_PROCESSID
          IMPORTING E_ENTITYS = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS
    )->GET_ENTITYS_ATTRIBUTES(
          EXPORTING I_ID_PROCESSID = I_ID_PROCESSID
          IMPORTING E_ENTITYS_ATTRIBUTES = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ENTITYS_ATRIBUTES
    )->GET_ACTIVITYS(
          EXPORTING I_ID_PROCESSID = I_ID_PROCESSID
          IMPORTING E_ACTIVITYS = ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_ACTIVITYS
    ).

  ENDMETHOD.


   METHOD zif_soft_expert_workflow~set_exec_activitys.

     DATA: ob_web_service TYPE REF TO zcl_webservice,
           lc_xml         TYPE string,
           i_name_file    TYPE string.

     r_instance = me.

     IF i_atividade = 'CANCELAR'.
       DATA(lc_activitys) = me->zif_soft_expert_workflow~at_activitys[].

       LOOP AT lc_activitys INTO DATA(wl_activitys).

         IF wl_activitys-actionsequence = '2'.
           wl_activitys-actionsequence = '3'.
           MODIFY lc_activitys FROM wl_activitys.
         ENDIF.

       ENDLOOP.

        DELETE lc_activitys WHERE sequencia_exec EQ space.

     ELSEIF i_atividade IS INITIAL.
       lc_activitys = me->zif_soft_expert_workflow~at_activitys[].
       DELETE lc_activitys WHERE sequencia_exec EQ space.
     ELSE.
       lc_activitys = me->zif_soft_expert_workflow~at_activitys[].
       DELETE lc_activitys WHERE activityid NE i_atividade.
     ENDIF.



     LOOP AT lc_activitys INTO DATA(wa_activitys).

       lc_xml = '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" xmlns:urn="urn:workflow">'.
       CONCATENATE lc_xml '<soapenv:Header/>' INTO lc_xml.
       CONCATENATE lc_xml '<soapenv:Body>' INTO lc_xml.
       CONCATENATE lc_xml '<urn:executeActivity>' INTO lc_xml.

       CONCATENATE lc_xml '<urn:WorkflowID>' i_workflowid '</urn:WorkflowID>' INTO lc_xml.
       CONCATENATE lc_xml '<urn:ActivityID>' wa_activitys-activityid '</urn:ActivityID>' INTO lc_xml.
       CONCATENATE lc_xml '<urn:ActionSequence>' wa_activitys-actionsequence '</urn:ActionSequence>' INTO lc_xml.
       CONCATENATE lc_xml '<urn:UserID>' i_new_workflow-userid '</urn:UserID>' INTO lc_xml.

       CONCATENATE lc_xml '</urn:executeActivity>' INTO lc_xml.
       CONCATENATE lc_xml '</soapenv:Body>' INTO lc_xml.
       CONCATENATE lc_xml '</soapenv:Envelope>' INTO lc_xml.

       CLEAR: e_activitys_retorno.


       CREATE OBJECT ob_web_service.

       TRY .
           ob_web_service->set_servico( i_servico = 'SE' ).
         CATCH zcx_webservice INTO DATA(lc_exception).
       ENDTRY.

       ob_web_service->set_tipo( i_tipo = '1' ).

       TRY .
           DATA(var_http) = ob_web_service->url( ). "Recupear qual é a URL que é preciso atribuir ao HEADER do WebService.
           DATA(lc_uri) = ob_web_service->get_uri(  ).
         CATCH zcx_webservice INTO lc_exception.
       ENDTRY.

       IF me->zif_soft_expert_workflow~at_object_inject IS BOUND.
         IF me->zif_soft_expert_workflow~at_object_inject->i_usuario IS NOT INITIAL.
           ob_web_service->set_usuario( i_usuario = CONV #( me->zif_soft_expert_workflow~at_object_inject->i_usuario ) ).
         ENDIF.
         IF me->zif_soft_expert_workflow~at_object_inject->i_senha IS NOT INITIAL.
           ob_web_service->set_senha( i_senha = CONV #( me->zif_soft_expert_workflow~at_object_inject->i_senha ) ).
         ENDIF.
       ENDIF.

       ob_web_service->zif_webservice~abrir_conexao( i_http = var_http i_autenticar = abap_true ).

       CALL METHOD ob_web_service->zif_webservice~consultar
         EXPORTING
           i_http                     = var_http
           i_xml                      = lc_xml
           i_not_content_length       = abap_true
         RECEIVING
           e_resultado                = DATA(xml_retorno)
         EXCEPTIONS
           http_communication_failure = 1
           http_invalid_state         = 2
           http_processing_failed     = 3
           http_invalid_timeout       = 4
           OTHERS                     = 5.

       CLEAR: ob_web_service.

       IF sy-subrc IS NOT INITIAL.
         RAISE EXCEPTION TYPE zcx_soft_expert_workflow
           EXPORTING
             textid = VALUE #( msgid = sy-msgid msgno = sy-msgno attr1 = CONV #( sy-msgv1 ) attr2 = CONV #( sy-msgv2 ) attr3 = CONV #( sy-msgv3 ) attr4 = CONV #( sy-msgv4 ) )
             msgid  = sy-msgid
             msgno  = sy-msgno
             msgty  = 'E'
             msgv1  = sy-msgv1
             msgv2  = sy-msgv2
             msgv3  = sy-msgv3
             msgv4  = sy-msgv4.
       ENDIF.

       DATA: arquivo TYPE REF TO zcl_arquivo.

       SELECT SINGLE * INTO @DATA(wa_zlest0007)
         FROM zlest0007
        WHERE id_ctg EQ 'XML_SE'
          AND prefix EQ 'XML'.

       IF sy-subrc IS INITIAL.
         CREATE OBJECT arquivo.
         CONCATENATE wa_zlest0007-pathunix 'executeActivity' i_new_workflow-processid wa_activitys-sequencia_exec '.xml' INTO i_name_file.

         OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
         IF sy-subrc IS INITIAL.
           TRANSFER lc_xml TO i_name_file.
           CLOSE DATASET i_name_file.
         ENDIF.

         CONCATENATE wa_zlest0007-pathunix 'executeActivityOut' i_new_workflow-processid wa_activitys-sequencia_exec '.xml' INTO i_name_file.

         OPEN DATASET i_name_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT .
         IF sy-subrc IS INITIAL.
           TRANSFER xml_retorno TO i_name_file.
           CLOSE DATASET i_name_file.
         ENDIF.

         CLEAR: arquivo.
       ENDIF.

       IF xml_retorno IS INITIAL.
         RAISE EXCEPTION TYPE zcx_soft_expert_workflow
           EXPORTING
             textid = VALUE #( msgid = zcx_soft_expert_workflow=>zcx_empty_return_webservice-msgid
                               msgno = zcx_soft_expert_workflow=>zcx_empty_return_webservice-msgno
                               attr1 = CONV #( lc_uri ) )
             msgid  = zcx_soft_expert_workflow=>zcx_empty_return_webservice-msgid
             msgno  = zcx_soft_expert_workflow=>zcx_empty_return_webservice-msgno
             msgv1  = CONV #( lc_uri )
             msgty  = 'E'.
       ENDIF.

       me->zif_soft_expert_workflow~set_exec_activitys_read_retorn(
          EXPORTING
            i_xml = xml_retorno
            i_uri = CONV #( lc_uri )
          IMPORTING
            e_exec_activity_workflow = e_activitys_retorno ).

       IF e_activitys_retorno-status NE 'SUCCESS'.
         me->zif_soft_expert_workflow~gera_erro_geral( EXPORTING i_texto = e_activitys_retorno-detail ).
       ENDIF.

     ENDLOOP.

   ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~SET_EXEC_ACTIVITYS_READ_RETORN.

    DATA: LC_XML_RET  TYPE REF TO CL_XML_DOCUMENT.

    CLEAR: E_EXEC_ACTIVITY_WORKFLOW.

    CREATE OBJECT LC_XML_RET.

    CALL METHOD LC_XML_RET->PARSE_STRING
      EXPORTING
        STREAM  = I_XML
      RECEIVING
        RETCODE = DATA(LC_TAMANHO).

    "Pega Tag Completa executeActivityResponse
    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'executeActivityResponse'
      RECEIVING
        NODE = DATA(LC_XML_NODE).

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'executeActivityResponse'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'executeActivityResponse'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Status'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'executeActivityResponse->Status'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'executeActivityResponse->Status'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EXEC_ACTIVITY_WORKFLOW-STATUS = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Code'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'executeActivityResponse->Code'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'executeActivityResponse->Code'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EXEC_ACTIVITY_WORKFLOW-CODE   = LC_XML_NODE->GET_VALUE( ).

    CALL METHOD LC_XML_RET->FIND_NODE
      EXPORTING
        NAME = 'Detail'
      RECEIVING
        NODE = LC_XML_NODE.

    IF NOT ( ( SY-SUBRC EQ 0 ) AND ( NOT LC_XML_NODE IS INITIAL ) ).
      RAISE EXCEPTION TYPE ZCX_SOFT_EXPERT_WORKFLOW
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
                            MSGNO = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
                            ATTR1 = 'executeActivityResponse->Detail'
                            ATTR2 = I_URI )
          MSGID  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGID
          MSGNO  = ZCX_SOFT_EXPERT_WORKFLOW=>ZCX_NO_LOCATE_TAG_WEBSERVICE-MSGNO
          MSGV1  = 'executeActivityResponse->Detail'
          MSGV2  = CONV #( I_URI )
          MSGTY  = 'E'.
    ENDIF.

    E_EXEC_ACTIVITY_WORKFLOW-DETAIL = LC_XML_NODE->GET_VALUE( ).

    LC_XML_RET->FREE( ).
    CLEAR: LC_XML_RET.

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~SET_PROCESS_WORKFLOW.

    R_INSTANCE = ME.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~SET_ATTIBUTES_WORKFLOW( EXPORTING I_ID_PROCESSID = I_ID_PROCESSID ).

  ENDMETHOD.


  METHOD ZIF_SOFT_EXPERT_WORKFLOW~SET_PROCESS_WORKFLOW_INJECT.

    R_INSTANCE = ME.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~AT_OBJECT_INJECT = I_INJECT.

    ME->ZIF_SOFT_EXPERT_WORKFLOW~SET_ATTIBUTES_WORKFLOW( EXPORTING I_ID_PROCESSID = I_INJECT->I_ID_PROCESSID ).

  ENDMETHOD.
ENDCLASS.
