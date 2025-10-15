interface ZIF_ITINERARIO
  public .


  class-data AT_IF_ITINERARIO type ref to ZIF_ITINERARIO .
  data AT_TVRO type TVRO .

  class-methods GET_INSTANCE
    returning
      value(R_IF_ITINERARIO) type ref to ZIF_ITINERARIO .
  methods GET_ITINERARIO_RELEVANTE
    importing
      !I_COD_LOC_COLETA type LIFNR
      !I_COD_LOC_ENTREGA type KUNNR
    exporting
      !E_TVRO type TVRO
    returning
      value(R_INSTANCE) type ref to ZIF_ITINERARIO
    raising
      ZCX_ITINERARIO .
  methods SET_ITINERARIO
    importing
      !I_ROUTE type ROUTE
    returning
      value(IF_ITINERARIO) type ref to ZIF_ITINERARIO
    raising
      ZCX_ITINERARIO .
  methods GET_DISTANCIA
    exporting
      !E_DISTANCIA type DISTZ
    returning
      value(IF_ITINERARIO) type ref to ZIF_ITINERARIO
    raising
      ZCX_ITINERARIO .
  methods GET_ITINERARIO_ZONAS
    importing
      !I_AZONE type AZONE
      !I_LZONE type LZONE
    exporting
      !E_TVRO type TVRO
    returning
      value(R_INSTANCE) type ref to ZIF_ITINERARIO
    raising
      ZCX_ITINERARIO .
endinterface.
