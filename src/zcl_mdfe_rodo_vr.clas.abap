class ZCL_MDFE_RODO_VR definition
  public
  create public .

*"* public components of class ZCL_MDFE_RODO_VR
*"* do not include other source files here!!!
public section.

  methods GET_CAPKG
    returning
      value(CAPKG) type NUM06 .
  methods GET_CAPM3
    returning
      value(CAPM3) type NUM03 .
  methods GET_CINT
    returning
      value(CINT) type CHAR10 .
  methods GET_CNPJ
    returning
      value(CNPJ) type STCD1 .
  methods GET_CPF
    returning
      value(CPF) type STCD2 .
  methods GET_IE
    returning
      value(IE) type CHAR14 .
  methods GET_PLACA
    returning
      value(PLACA) type CHAR07 .
  methods GET_RENAVAM
    returning
      value(RENAVAM) type CHAR11 .
  methods GET_RNTRC
    returning
      value(RNTRC) type NUM08 .
  methods GET_TARA
    returning
      value(TARA) type NUM06 .
  methods GET_TPCAR
    returning
      value(TPCAR) type NUM02 .
  methods GET_TPPROP
    returning
      value(TPPROP) type NUM1 .
  methods GET_UF
    returning
      value(UF) type ZCHAR02 .
  methods GET_UF_PROP
    returning
      value(UF_PROP) type ZCHAR02 .
  methods GET_XNOME
    returning
      value(XNOME) type CHAR060 .
  methods SET_CAPKG
    importing
      !CAPKG type NUM06 .
  methods SET_CAPM3
    importing
      !CAPM3 type NUM03 .
  methods SET_CINT
    importing
      !CINT type CHAR10 .
  methods SET_CNPJ
    importing
      !CNPJ type STCD1 .
  methods SET_CPF
    importing
      !CPF type STCD2 .
  methods SET_IE
    importing
      !IE type CHAR14 .
  methods SET_PLACA
    importing
      !PLACA type CHAR07 .
  methods SET_RENAVAM
    importing
      !RENAVAM type CHAR11 .
  methods SET_RNTRC
    importing
      !RNTRC type NUM08 .
  methods SET_TARA
    importing
      !TARA type NUM06 .
  methods SET_TPCAR
    importing
      !TPCAR type NUM02 .
  methods SET_TPPROP
    importing
      !TPPROP type NUM1 .
  methods SET_UF
    importing
      !UF type ZCHAR02 .
  methods SET_UF_PROP
    importing
      !UF_PROP type ZCHAR02 .
  methods SET_XNOME
    importing
      !XNOME type CHAR060 .
protected section.
*"* protected components of class ZCL_MDFE_RODO_VR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_MDFE_RODO_VR
*"* do not include other source files here!!!

  data AT_CINT type CHAR10 .
  data AT_PLACA type CHAR7 .
  data AT_RENAVAM type CHAR11 .
  data AT_TARA type NUM06 .
  data AT_CAPKG type NUM06 .
  data AT_CAPM3 type NUM03 .
  data AT_CPF type STCD2 .
  data AT_CNPJ type STCD1 .
  data AT_RNTRC type NUM08 .
  data AT_XNOME type CHAR060 .
  data AT_IE type CHAR14 .
  data AT_UF_PROP type CHAR2 .
  data AT_TPPROP type NUM1 .
  data AT_TPCAR type NUM02 .
  data AT_UF type CHAR2 .
ENDCLASS.



CLASS ZCL_MDFE_RODO_VR IMPLEMENTATION.


method GET_CAPKG.
  CAPKG = ME->AT_CAPKG.
endmethod.


method GET_CAPM3.
  CAPM3 = ME->AT_CAPM3.
endmethod.


method GET_CINT.
  CINT = ME->AT_CINT.
endmethod.


method GET_CNPJ.
  CNPJ = ME->AT_CNPJ.
endmethod.


method GET_CPF.
  CPF = ME->AT_CPF.
endmethod.


method GET_IE.
  IE = ME->AT_IE.
endmethod.


method GET_PLACA.
  PLACA = ME->AT_PLACA.
endmethod.


method GET_RENAVAM.
  RENAVAM = ME->AT_RENAVAM.
endmethod.


method GET_RNTRC.
  RNTRC = ME->AT_RNTRC.
endmethod.


method GET_TARA.
  TARA = ME->AT_TARA.
endmethod.


method GET_TPCAR.
  TPCAR = ME->AT_TPCAR.
endmethod.


method GET_TPPROP.
  TPPROP = ME->AT_TPPROP.
endmethod.


method GET_UF.
  UF = ME->AT_UF.
endmethod.


method GET_UF_PROP.
  UF_PROP = ME->AT_UF_PROP.
endmethod.


method GET_XNOME.
  XNOME = ME->AT_XNOME.
endmethod.


method SET_CAPKG.
   ME->AT_CAPKG = CAPKG.
endmethod.


method SET_CAPM3.
   ME->AT_CAPM3 = CAPM3.
endmethod.


method SET_CINT.
  ME->AT_CINT = CINT.
endmethod.


method SET_CNPJ.
  ME->AT_CNPJ = CNPJ.
endmethod.


method SET_CPF.
  ME->AT_CPF = CPF.
endmethod.


method SET_IE.
   ME->AT_IE = IE.
endmethod.


method SET_PLACA.
   ME->AT_PLACA = PLACA.
endmethod.


method SET_RENAVAM.
  ME->AT_RENAVAM = RENAVAM.
endmethod.


method SET_RNTRC.
  ME->AT_RNTRC = RNTRC.
endmethod.


method SET_TARA.
  ME->AT_TARA = TARA.
endmethod.


method SET_TPCAR.
  ME->AT_TPCAR = TPCAR.
endmethod.


method SET_TPPROP.
   ME->AT_TPPROP = TPPROP.
endmethod.


method SET_UF.

  ME->AT_UF = UF.
endmethod.


method SET_UF_PROP.
  ME->AT_UF_PROP = UF_PROP.
endmethod.


method SET_XNOME.
  ME->AT_XNOME = XNOME.
endmethod.
ENDCLASS.
