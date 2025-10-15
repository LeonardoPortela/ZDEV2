interface ZIF_SCREEN
  public .


  class-data SPLIT type ref to CL_GUI_SPLITTER_CONTAINER .

  class-methods SET_CRIAR_TELA_PADRAO_REPORT
    importing
      !I_TITULO type STRING optional
      !I_FILTROS type ZIF_SCREEN_LINHA_FILTRO_T optional
    changing
      value(SPLIT) type ref to CL_GUI_SPLITTER_CONTAINER optional
      value(HTML) type ref to CL_GUI_HTML_VIEWER optional
      value(ALV) type ref to CL_GUI_ALV_GRID optional
    returning
      value(R_CRIOU) type CHAR01 .
  class-methods GET_PEGA_IMAGEM_URL
    importing
      !I_NOME_LOGO type TDOBNAME
    returning
      value(R_URL) type CHAR255 .
endinterface.
