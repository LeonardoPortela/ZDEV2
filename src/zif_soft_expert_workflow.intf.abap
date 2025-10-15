interface ZIF_SOFT_EXPERT_WORKFLOW
  public .


  data AT_WORKFLOW type ZSEXPT00001 .
  data AT_ATRIBUTES type ZDE_ZSEXPT00002_T .
  data AT_ENTITYS type ZDE_ZSEXPT00003_T .
  data AT_ENTITYS_ATRIBUTES type ZDE_ZSEXPT00004_T .
  data AT_ACTIVITYS type ZDE_ZSEXPT00005_T .
  data AT_ATTACHMENT type ZDE_ZSEXPT00007_T .
  constants ST_TYPE_FIELD_VALUE_TEXTO type ZDE_SE_TYPES value '00' ##NO_TEXT.
  constants ST_TYPE_FIELD_VALUE_NUMERICO type ZDE_SE_TYPES value '01' ##NO_TEXT.
  constants ST_TYPE_FIELD_VALUE_MOEDA type ZDE_SE_TYPES value '02' ##NO_TEXT.
  constants ST_TYPE_FIELD_VALUE_DATA type ZDE_SE_TYPES value '03' ##NO_TEXT.
  constants ST_TYPE_FIELD_VALUE_HORA type ZDE_SE_TYPES value '04' ##NO_TEXT.
  data AT_OBJECT_INJECT type ref to ZIF_SOFT_EXPERT_WS_INJECT .
  class-data AT_SOFT_EXPERT type ref to ZIF_SOFT_EXPERT_WORKFLOW .
  constants ST_PROCESS_MANUTENCAO_ROMANEIO type ZDE_PROCESSID_SAP value '01' ##NO_TEXT.
  constants ST_PROCESS_SOLICITACAO_MIRO type ZDE_PROCESSID_SAP value '02' ##NO_TEXT.

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW .
  class-methods GET_PROCESSID_SE
    importing
      !I_PROCESSID_SAP type ZDE_PROCESSID_SAP
    exporting
      !E_ZSEXPT00006 type ZSEXPT00006
    returning
      value(R_PROCESSID) type ZDE_SE_PROCESSID
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods CANCEL_WORKFLOW
    importing
      !I_WORKFLOWID type STRING
      !I_EXPLANATION type STRING
    exporting
      !E_CANCEL_WORFLOW_RET type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods CANCEL_WORKFLOW_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_CANCEL_WORKFLOW type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods CLEAR
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW .
  methods CREATE_NEW_WORKFLOW
    importing
      !I_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET optional
      !I_DESATIVAR type CHAR01 optional
    exporting
      !E_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ATTRIBUTES
    importing
      !I_WORKFLOWID type STRING
    exporting
      !E_EDIT_ATTRIBUTES_RETORNO type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods NEW_ATTACHMENTS
    importing
      !I_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW
      !I_WORKFLOWID type STRING
    exporting
      !E_NEW_ATTACHMENTS_RETORNO type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ATTRIBUTES_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_EDIT_ATTRIBUTES_RETORNO type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ATTRIBUTES_VALUE_XML
    importing
      !I_TABELA type STRING
      !I_CAMPO type STRING
      !I_VALOR type STRING
    changing
      !E_XML type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_EDITENTITYS
    importing
      !I_WORKFLOWID type STRING
    exporting
      !E_EDIT_ENTITY_RETORNO type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ENTITYS_NEWCHILD_VALUE_RT
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_NEW_CHILD_RETORNO type ZDE_SE_NEW_CHILD_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ENTITYS_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_EDIT_ENTITY_RETORNO type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ENTITY_CHILD_VALUE_XML
    importing
      !I_ENTITYID type STRING
      !I_TABELA type STRING
      !I_CAMPO type STRING
      !I_VALOR type STRING
    changing
      !E_XML type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ENTITY_NEWCHILD_VALUE_XML
    importing
      !I_ENTITYID type STRING
      !I_TABELA type STRING
      !I_CAMPO type STRING
      !I_VALOR type STRING
    changing
      !E_XML type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods EDIT_ENTITY_VALUE_XML
    importing
      !I_ENTITYID type STRING
      !I_TABELA type STRING
      !I_CAMPO type STRING
      !I_VALOR type STRING
      !I_ATTRIBUTEID type STRING optional
      !I_TYPE type ZDE_SE_TYPES optional
    changing
      !E_XML type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ACTIVITYS
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    exporting
      !E_ACTIVITYS type ZDE_ZSEXPT00005_T
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ATRIBUTES
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    exporting
      !E_ATRIBUTES type ZDE_ZSEXPT00002_T
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ATTRIBUTES_VALUES
    exporting
      !E_XML_ATRIBUTOS type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    exporting
      !E_ENTITYS type ZDE_ZSEXPT00003_T
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS_ATTRIBUTES
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    exporting
      !E_ENTITYS_ATTRIBUTES type ZDE_ZSEXPT00004_T
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS_CHILD_VALUES
    importing
      !I_ENTITYID type STRING
      !I_INDEX type I
      !I_RELATIONSHIP type STRING
    exporting
      !E_XML_ENTITY_CHILD type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS_VALUES
    importing
      !I_ENTITYID type STRING
      !I_INDEX type I
    exporting
      !E_XML_ENTITY type STRING
      !E_RELATIONSHIP type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_RELATIONSHIPS_VALUES
    importing
      !I_ENTITYID type STRING
      !I_INDEX type I
    exporting
      !E_RELATIONSHIP type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_MANY_EDITENTITYS_RECORDS
    importing
      !I_ENTITYID type STRING
      !I_RELATIONSHIP type STRING optional
    exporting
      !E_MANY type I
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_PROCESSID
    exporting
      !E_ID_PROCESSID type ZDE_SE_PROCESSID
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_USERID
    importing
      !I_USUARIO type SY-UNAME default SY-UNAME
    exporting
      !E_USERID type J_1BCPF
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_WORKFLOW
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    exporting
      !E_WORKFLOW type ZSEXPT00001
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_WORKFLOWTITLE
    exporting
      !E_WORKFLOWTITLE type ZDE_SE_WORKFLOWTITLE
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods NEW_WORKFLOW
    importing
      !I_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW
    exporting
      !E_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods NEW_ATTACHMENTS_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_NEW_ATTACHMENT type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods NEW_WORKFLOW_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_EXEC_ACTIVITYS
    importing
      !I_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW
      !I_WORKFLOWID type STRING
      !I_ATIVIDADE type ZDE_SE_ACTIVITYID optional
    exporting
      !E_ACTIVITYS_RETORNO type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_EXEC_ACTIVITYS_READ_RETORN
    importing
      !I_XML type STRING
      !I_URI type STRING
    exporting
      !E_EXEC_ACTIVITY_WORKFLOW type ZDE_SE_CANCEL_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_PROCESS_WORKFLOW
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_PROCESS_WORKFLOW_INJECT
    importing
      !I_INJECT type ref to ZIF_SOFT_EXPERT_WS_INJECT
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_ATTIBUTES_WORKFLOW
    importing
      !I_ID_PROCESSID type ZDE_SE_PROCESSID
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods SET_ATTACHMENT
    importing
      !I_ATTACHMENTS type ZDE_ZSEXPT00007_T
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods CANCEL_WORKFLOW_NOVO
    importing
      !I_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET optional
    exporting
      !E_NEW_WORKFLOW type ZDE_SE_NEW_WORFLOW_RET
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WORKFLOW
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
endinterface.
