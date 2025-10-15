*"* components of interface ZIF_TAXA_CURVA_DB
interface ZIF_TAXA_CURVA_DB
  public .


  methods INSERIR
    importing
      !OBJ_TAXA type ref to ZCL_TAXA_CURVA
      !NEGATIVA type C optional .
  methods ATUALIZAR
    importing
      !OBJ_TAXA type ref to ZCL_TAXA_CURVA .
  methods ATUALIZAR_FRAME
    importing
      !OBJ_TAXA type ref to ZCL_TAXA_CURVA .
  methods INSERIR_FRAME
    importing
      !I_ZSDT0059 type ZSDT0059
      !I_ZSDT0055 type ZSDT0055
      !I_AUART type AUART optional
      !I_DATA type DATS optional .
  methods INSERIR_IN
    importing
      !OBJ_TAXA type ref to ZCL_TAXA_CURVA .
endinterface.
