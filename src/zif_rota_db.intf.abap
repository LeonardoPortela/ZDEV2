*"* components of interface ZIF_ROTA_DB
interface ZIF_ROTA_DB
  public .


  methods INSERIR
    importing
      !OBJ_ROTA type ref to ZCL_ROTA
    raising
      ZCX_WEBSERVICE .
  methods DELETAR
    importing
      !OBJ_ROTA type ref to ZCL_ROTA .
  methods ATUALIZAR
    importing
      !OBJ_ROTA type ref to ZCL_ROTA .
endinterface.
