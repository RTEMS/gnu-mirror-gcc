/* { dg-skip-if "too many arguments in function call" { bpf-*-* } } */

typedef __SIZE_TYPE__ size_t;
typedef void *XtPointer;

typedef struct _WidgetRec *Widget;
typedef struct _WidgetClassRec *WidgetClass;

extern WidgetClass commandWidgetClass;

typedef void (*XtCallbackProc)(
    Widget 		 ,
    XtPointer 		 ,	 
    XtPointer 		 	 
);

extern const  char XtStrings[];


typedef struct						 
{
	char			*Name,			 
				*Label;			 
	XtCallbackProc		Callback;		 
	XtPointer		ClientData;		 
	Widget			W;				 
} DialogButtonType, *DialogButtonTypePtr;

 
Widget AddButtons(Widget Parent, Widget Top,
	DialogButtonTypePtr Buttons, size_t Count)
{
	int		i;

	for (i = 0; i < Count; i++)
	{
		if (!Buttons[i].Label)
			continue;
#ifdef __GCC_ARM_CAPABILITY_ANY
		Buttons[i].W  = (__intcap_t)XtVaCreateManagedWidget(Buttons[i].Name,
#else
		Buttons[i].W  = XtVaCreateManagedWidget(Buttons[i].Name,
#endif
			commandWidgetClass,
			Parent,
			((char*)&XtStrings[429]) , Buttons[i].Label,
			"fromHoriz" , i ? Buttons[i-1].W : ((void *)0) ,
			"fromVert" , Top,
			"resizable" , 1 ,
			((void *)0) );

		XtAddCallback(((char*)&XtStrings[136]),
				 Buttons[i].Callback, Buttons[i].ClientData);
	}
	return(Buttons[Count-1].W);
}

