HLD : dialog {
      label = " HLC v1.0 - Hydraulic Losses Calculator";
	   
	   : spacer { width = 1;  height = 0.3;  } 
	
       : column {
        fixed_width = true;
	   //height = 50;
	   //width = 62;
         : column {
		 :row{
		 :text {
		 key = "fltext";
		 value = "Total Friction Losses (m):";
		 width=10;
		 }
		 :edit_box {
		 key = "tot_fric_loss";
		 label = "";
		 edit_width = 20;
		 }
		 }
		 :row{
		
         :text {
		 key = "text1";
		 value = "K fittings total :";
		 width=10;
		 }
		 :edit_box {
		 key = "disp";
		 edit_width = 20;
		 }
		 }
		:row{
		 :text {
		 key = "tfltext";
		 value = "Total Fittings Losses (m):";
		 width=10;
		 }
		  :edit_box {
		 key = "tot_fitt_loss";
		 label = "";
		 edit_width = 20;
		 }
		 }
		 :row{
		 :text {
		 key = "tltext";
		 value = "Total Losses (m):";
		 width=10;
		 }
		 :edit_box {
		 key = "tot_loss";
		 label = "";
		 edit_width = 20;
		 }
		 }
		 :row{
		 :text {
		 key = "hgtext";
		 value = "Hydraulic Gradient \U+2030:";
		 width=10;
		 }
		 :edit_box {
		 key = "hyd_grad";
		 label = "";
		 edit_width = 20;
		 }
		 }
		 : spacer { width = 0.2;  height = 0.2;  } 
		
		    :row{
			 : button {
               key = "SHA";
               label = "Select &Hor. Alignment";
               is_default = false;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			   : button {
               key = "SVA";
               label = "Select &Ver. Alignment";
               is_default = false;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  }

			  :boxed_column{children_fixed_height=true; children_fixed_width=true; 
             label = "Number of Elbows";			
			:row{ alignment=right;
			
			:text {
		            key = "telbow90";
		            value = "90";
					allow_accept = true;
		            fixed_width = true;
		            width=6;
		          }
			:spacer{width=0.05;}
			:text {
		            key = "telbow45";
		            value = "45";
		            width=4;
		          }	  
		    :spacer{width=1;}	 
			:text {
		            key = "telbow22.5";
		            value = "22.5";
		            width=4;
		          }	
           	:spacer{width=1;}	  
			:text {
		            key = "telbow11.25";
		            value = "11.25";
		            width=5;
		          }	
			:spacer{width=0.5;}		  
				:text {
		            key = "telbowns";
		            value = "N.Std";
		            width=6;
		          }  
				  
			    }  			
			:row{alignment=right;
	        :column{alignment=left;
	        : button {
               key = "nelbowoverride";
               label = "&OVR";
               is_default = false;
               fixed_width = true;
			   width = 4;
			   alignment = centered;
		           }
			 }			
			:  edit_box {
                           key = "nb90";
                           label = ""; 
                           edit_width = 6;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "nb45";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "nb22";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "nb11";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }	
            :  edit_box {
                           key = "nbnsd";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }									
						
                 }
				 
						
			 }
			  
			  
		 : boxed_column {
             label = "";
			 :row{children_fixed_height=true; children_fixed_width=true; 
			  :column{alignment=right;
			 :text {
		            key = "lpipetext";
		            value = "Length of pipe (m):";
		            width=18;
			      }
				  }
		   :spacer{width=14;}
		   
		   :column{alignment=right;
		    : button {
               key = "pipelengthoverride";
               label = "OV&R";
               is_default = false;
               fixed_width = true;
			   width = 4;
			  // height = 2;
			   alignment = left;
	          }
			 }
			 :column{alignment=right;
			 :  edit_box {
                           key = "pipelength";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
						
			}
			}
		     : edit_box {
               key = "diameter";
               label = "Internal Dia. (mm):";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			:  edit_box {
                           key = "angtol";
                           label = "Allowable deflection angle (degree):"; 
						   allow_accept = true;
                           edit_width = 5;
                           value = "";
					    }
			: spacer { width = 1;  height = 0.3;  } 			
            
            :column{children_fixed_height=true; children_fixed_width=true; 
             //label = "Pipe Flow";			
			:row{ alignment=right;
			
			:text {
		            key = "Q_unit1";
		            value = "(m3/day)";
					allow_accept = true;
		            fixed_width = true;
		            width=7;
		          }
			:spacer{width=0.05;}
			:text {
		            key = "Q_unit2";
		            value = "(m3/hr)";
		            width=6;
		          }	  
		    :spacer{width=0.25;}	 
			:text {
		            key = "Q_unit3";
		            value = "(gpm)";
		            width=5;
		          }	
           	:spacer{width=1.5;}	  
			:text {
		            key = "Q_unit4";
		            value = "(l/s)";
		            width=5;
		          }	  
				  
				  
			    }  			
			:row{alignment=right;
			:column{alignment=left;
			:text {
		            key = "flowtext";
		            value = "Flow (Q) :";
					fixed_width = true;
		            width=18;
		          }
			}	  
			:  edit_box {
                           key = "flow_cumd";
                           label = ""; 
                           edit_width = 6;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "flow_cumhr";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "flow_gpm";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "flow_lps";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }			
						
                 }						
						
			 }
			 :row{
		
         :text {
		 key = "vel_text";
		 value = "Velocity (m/s):";
		 width=7;
		 }
		 :spacer{width=0.1;}	
		 : button {
               key = "goalseek";
               label = "&Goal Seek";
               is_default = false;
               fixed_width = true;
			   width = 5;
			  // height = 2;
			   alignment = centered;
	          }
			  
			 :  edit_box {
                           key = "velocity";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
						}
			: popup_list {				//define popup list
                          label = "Pipe Material:";			//give it a label
                          key = "material_selections";			//give it a name
                          value = "5" ;				//initial value
                          edit_width = 19;			//fix the width
                         }					//end list
			
			 
			 :row{
			 	 : button {
               key = "kccoeff";
               label = "&Set K and C Coefficients";
               is_default = false;
               fixed_width = true;
			   width = 5;
			  // height = 2;
			   alignment = centered;
	          }
			 
			  :text {
		             key = "ccoefftext";
		             value = "C coeff:";
		             width=5;
		            }
			:  edit_box {
                           key = "ccoeff";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }		
		
            }
			}
			
			 
			 : spacer { width = 1;  height = 0.0;  } 
			 
		 : boxed_column {
             label = "Number of Valves and Tees";
			 
			 : edit_box {
               key = "nbfv";
               label = "Number of Butterfly Valves:";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			  : edit_box {
               key = "ngv";
               label = "Number of Gate Valves:";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			  : edit_box {
               key = "nglv";
               label = "Number of Globe Valves:";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			   : edit_box {
               key = "nav";
               label = "Number of Angle Valves:";
			  allow_accept = true;
               edit_width = 5;
               value = "";
             }
			 
			: edit_box {
               key = "ntr";
               label = "Number of Tees through run:";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			 
			: edit_box {
               key = "ntb";
               label = "Number of Tees through branch:";
			   allow_accept = true;
               edit_width = 5;
               value = "";
             }
			 
			 
			 }
			 
			  
			   //: spacer { width = 1;  height = 0.0;  } 
			   : row{
             : button {
               key = "cancel";
               label = "&Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			   : button {
               key = "calc";
               label = "Calculate";
               fixed_width = true;
			   is_default = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  
			   : button {
               key = "about";
               label = "&About";
               fixed_width = true; 
			   width = 15;
			  // height = 2;
			   alignment = centered;

             }
			  }
			 
       }
}	   
}
////////////////////////////////////////////////
nest1 : dialog {
        label = "K and C coeff values";
             : column {
				  : boxed_column {
             label = "K values";
		     : edit_box {
               key = "k90de";
               label = "K value of 90 degree elbow:";
               edit_width = 5;
               value = "";
             }
			   : edit_box {
               key = "k45de";
               label = "K value of 45 degree elbow:";
               edit_width = 5;
               value = "";
             }
			 
			    : edit_box {
               key = "k22de";
               label = "K value of 22.5 degree elbow:";
               edit_width = 5;
               value = "";
             }
			     : edit_box {
               key = "k11de";
               label = "K value of 11.25 degree elbow:";
               edit_width = 5;
               value = "";
             }
			 
			  : edit_box {
               key = "kbfv";
               label = "K value of Butterfly Valve:";
               edit_width = 5;
               value = "";
             }
			  : edit_box {
               key = "kgv";
               label = "K value of Gate Valve:";
               edit_width = 5;
               value = "";
             }
			  : edit_box {
               key = "kglv";
               label = "K value of Globe Valve:";
               edit_width = 5;
               value = "";
             }
			   : edit_box {
               key = "kav";
               label = "K value of Angle Valve:";
               edit_width = 5;
               value = "";
             }
			 
			: edit_box {
               key = "ktr";
               label = "K value of Tee through run:";
               edit_width = 5;
               value = "";
             }
			 
			: edit_box {
               key = "ktb";
               label = "K value of Tee through branch:";
               edit_width = 5;
               value = "";
             }
			 
			 }
		 : boxed_column {
         label = "Hazen-Williams C Coeff values";
		    : edit_box {
                        key = "cdin";
                        label = "C coeff of new Ductile iron pipe:";
                        edit_width = 5;
                        value = "";
             }
 		    : edit_box {
                        key = "cdio";
                        label = "C coeff of old D.I pipe:";
                        edit_width = 5;
                        value = "";
             }
		    : edit_box {
                        key = "cpvc";
                        label = "C coeff of PVC pipe:";
                        edit_width = 5;
                        value = "";
             }
		  : edit_box {
                        key = "chdpe";
                        label = "C coeff of HDPE pipe:";
                        edit_width = 5;
                        value = "";
             }
		  : edit_box {
                        key = "ccs";
                        label = "C coeff of C.S pipe:";
                        edit_width = 5;
                        value = "";
             }
		  : edit_box {
                        key = "cac";
                        label = "C coeff of A.C pipe:";
			            edit_width = 5;
                        value = "";
             }
			  : edit_box {
                        key = "cfrp";
                        label = "C coeff of FRP pipe:";
			            edit_width = 5;
                        value = "";
             }
			  : edit_box {
                        key = "cwi";
                        label = "C coeff of W.I pipe:";
			            edit_width = 5;
                        value = "";
             }
			  : edit_box {
                        key = "cc";
                        label = "C coeff of Copper pipe:";
			            edit_width = 5;
                        value = "";
             }	
			  : edit_box {
                        key = "cb";
                        label = "C coeff of Brass pipe:";
			            edit_width = 5;
                        value = "";
             }	
			  : edit_box {
                        key = "cci";
                        label = "C coeff of C.I pipe:";
			            edit_width = 5;
                        value = "";
             }	
			  : edit_box {
                        key = "cclp";
                        label = "C coeff of Cement-lined pipe:";
			            edit_width = 5;
                        value = "";
             }			 
			 
			 }
			 	 : row{
				 
			: button {
               key = "resetdefaults";
               label = "&Reset";
               is_default = false;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
				 
			  : button {
               key = "setasdefaults";
               label = "&Set as defaults";
               is_default = false;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			
			  : button {
               key = "cancel1";
               label = "&Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			    : button {
               key = "OK2";
               label = "&OK";
               is_default = true;
			   allow_accept = true;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  }
		
			  }
			  
			 
	}
////////////////////////////////////////////////
nest2 : dialog {
        label = "Goal Seek Velocity";
             : column {
			 : edit_box {
                        key = "gsvelocity";
                        label = "Required velocity (m/s)";
			            allow_accept = true;
                        edit_width = 5;
                        value = "";
             }
			 
			 :row{
			  : button {
               key = "cancel2";
               label = "Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			    : button {
               key = "OK22";
               label = "OK";
               is_default = true;
			    allow_accept = true;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  }
			  }
			 
	 }
////////////////////////////////////////////////
nest3 : dialog {
        label = "Override number of elbows";
            :column{children_fixed_height=true; children_fixed_width=true; 
             label = "";			
			:row{ alignment=right;
			
			:text {
		            key = "otelbow90";
		            value = "90";
					allow_accept = true;
		            fixed_width = true;
		            width=6;
		          }
			:spacer{width=0.05;}
			:text {
		            key = "otelbow45";
		            value = "45";
		            width=4;
		          }	  
		    :spacer{width=1;}	 
			:text {
		            key = "otelbow22.5";
		            value = "22.5";
		            width=4;
		          }	
           	:spacer{width=1;}	  
			:text {
		            key = "otelbow11.25";
		            value = "11.25";
		            width=5;
		          }	
			  :spacer{width=0.5;}
				  
			    }  			
			:row{alignment=right;
			 :column{alignment=left;
	        : button {
               key = "oreset";
               label = "&Reset";
               is_default = false;
               fixed_width = true;
			   width = 4;
			   alignment = centered;
		           }
			 }			
			:  edit_box {
                           key = "onb90";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "onb45";
                           label = ""; 
                           edit_width =5;
						   alignment = left;
                           value = "";
					    }
			:  edit_box {
                           key = "onb22";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }
			
			:  edit_box {
                           key = "onb11";
                           label = ""; 
                           edit_width = 5;
						   alignment = left;
                           value = "";
					    }	
           									
						
                 }						
						
			 }
			 :row{alignment=centered;
			  : button {
               key = "cancel3";
               label = "&Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			    : button {
               key = "OK3";
               label = "&OK";
               is_default = true;
			    allow_accept = true;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  }
				 
	 }
////////////////////////////////////////////////
nest4 : dialog {
        label = "Pipe Length Override";
             : column {
			 : edit_box {
                        key = "plo";
                        label = "Pipe length (m)";
			            allow_accept = true;
                        edit_width = 5;
                        value = "";
             }
			 
			 :row{
			  : button {
               key = "cancel4";
               label = "&Cancel";
               is_default = false;
               is_cancel = true;
			   fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			    : button {
               key = "OK4";
               label = "&OK";
               is_default = true;
               fixed_width = true;
			   width = 15;
			  // height = 2;
			   alignment = centered;
	          }
			  }
			  }
			 
	 }