FUNCTION-POOL zsd_liberacao_ov.             "MESSAGE-ID ..

* INCLUDE LZSD_LIBERACAO_OVD...              " Local class definition

DATA gt_text TYPE STANDARD TABLE OF char0241.
DATA go_container TYPE REF TO cl_gui_custom_container.
DATA go_editor TYPE REF TO cl_gui_textedit.

DATA gv_canc.
DATA gv_title TYPE sytitle.
DATA gv_limit TYPE i.
DATA gv_edit TYPE flag.
