interface ZIF_SQL_API_PYTHON
  public .


  class-data AT_SQL_API_PYTHON type ref to ZIF_SQL_API_PYTHON .
  data AT_ZWAT0003 type ZWAT0003 .

  class-methods GET_INSTANCE
    returning
      value(R_INSTANCE) type ref to ZIF_SQL_API_PYTHON
    raising
      ZCX_SQL_API_PYTHON .
  methods EXEC_SCRIPT
    importing
      !I_SCRIPT type STRING
    exporting
      !E_DATA type DATA
    returning
      value(R_SQL_API_PYTHON) type ref to ZIF_SQL_API_PYTHON
    raising
      ZCX_SQL_API_PYTHON
      ZCX_ERROR .
  methods EXEC_SQL
    importing
      !I_SQL type STRING
    exporting
      !E_DATA type DATA
    returning
      value(R_SQL_API_PYTHON) type ref to ZIF_SQL_API_PYTHON
    raising
      ZCX_SQL_API_PYTHON
      ZCX_ERROR .
  methods SET_HTTP
    importing
      !I_URL type STRING
      !I_JSON type STRING
      !I_USUARIO type STRING optional
      !I_SENHA type STRING optional
    exporting
      !E_CDATA type STRING
    returning
      value(R_SQL_API_PYTHON) type ref to ZIF_SQL_API_PYTHON
    raising
      ZCX_SQL_API_PYTHON
      ZCX_ERROR .
  methods GETXMLNFE
    importing
      !I_CHAVE type STRING
    exporting
      !E_XML type STRING
    returning
      value(R_SQL_API_PYTHON) type ref to ZIF_SQL_API_PYTHON .
  methods GETXMLCTE
    importing
      !I_CHAVE type STRING
    exporting
      !E_XML type STRING
    returning
      value(R_SQL_API_PYTHON) type ref to ZIF_SQL_API_PYTHON .
endinterface.
