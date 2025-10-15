*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMM_XML_PO_COUPA................................*
DATA:  BEGIN OF STATUS_ZMM_XML_PO_COUPA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZMM_XML_PO_COUPA              .
CONTROLS: TCTRL_ZMM_XML_PO_COUPA
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZMM_XML_PO_COUPA              .
TABLES: ZMM_XML_PO_COUPA               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
