interface ZIF_SOFT_EXPERT_WS_INJECT
  public .


  data I_ID_PROCESSID type ZDE_SE_PROCESSID .
  data I_USUARIO type ZDE_USUARIO .
  data I_SENHA type ZDE_SENHA .
  class-data AT_INJECT type ref to ZIF_SOFT_EXPERT_WS_INJECT .

  class-methods GET_INSTANCE
    importing
      !I_WORKFLOW_SAP type ZDE_PROCESSID_SAP optional
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ATTRIBUTES_VALUES
    importing
      !I_WORKFLOW type ref to ZIF_SOFT_EXPERT_WORKFLOW
    exporting
      !E_XML_ATRIBUTOS type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS_VALUES
    importing
      !I_WORKFLOW type ref to ZIF_SOFT_EXPERT_WORKFLOW
      !I_ENTITYID type STRING
      !I_INDEX type I
    exporting
      !E_XML_ENTITY type STRING
      !E_RELATIONSHIP type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_RELATIONSHIPS_VALUES
    importing
      !I_WORKFLOW type ref to ZIF_SOFT_EXPERT_WORKFLOW
      !I_ENTITYID type STRING
      !I_INDEX type I
    exporting
      !E_RELATIONSHIP type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_MANY_EDITENTITYS_RECORDS
    importing
      !I_WORKFLOW type ref to ZIF_SOFT_EXPERT_WORKFLOW
      !I_ENTITYID type STRING
      !I_RELATIONSHIP type STRING optional
    exporting
      !E_MANY type I
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_ENTITYS_CHILD_VALUES
    importing
      !I_WORKFLOW type ref to ZIF_SOFT_EXPERT_WORKFLOW
      !I_ENTITYID type STRING
      !I_INDEX type I
      !I_RELATIONSHIP type STRING
    exporting
      !E_XML_ENTITY_CHILD type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods GET_WORKFLOW_TITLE
    exporting
      !E_WORKFLOW_TITLE type STRING
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT
    raising
      ZCX_SOFT_EXPERT_WORKFLOW .
  methods CLEAR
    returning
      value(R_INSTANCE) type ref to ZIF_SOFT_EXPERT_WS_INJECT .
endinterface.
