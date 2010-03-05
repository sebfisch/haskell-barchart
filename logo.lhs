> {-# LANGUAGE ParallelListComp #-}
>
> import Graphics.Rendering.Diagrams
>
> main = renderAs PNG "logo.png" (Width 400) (pad 1 1 logo)
>
> logo = hcatA bottom (yaxis:hspace 1:zipWith ($) [b,a,r,c,h,a,r,t] colors)
>     // xaxis
>  where
>   xaxis = straight $ pathFromVectors [(30,0)]
>   yaxis = straight $ pathFromVectors [(0,6)]
>
> colors = cycle [seagreen,firebrick,midnightblue]
>
> b col = hcatA bottom [fillColor col $ roundRectF 1 5 0.1,
>                       translateX (-1) $ arc 1.5 0.6 0.4]
>
> a col = translateX (-1) $
>         hcatA bottom [translateX 0.5 $ arc 1.2 0.1 0.9,
>                      fillColor col $ roundRectF 1 2.7 0.1]
>
> r col = translateY 1 $
>         hcatA top [fillColor col $ roundRectF 1 3 0.1,
>                    translateX (-0.5) $ arc 2 0.5 0.75]
>
> c col = translateX (-2.5) $ arc 1.5 0.1 0.9
>
> h col = translateX (-1.5) $
>         hcatA bottom [fillColor col $ roundRectF 1 6 0.1,
>                       vcatA right [translateY 1 $ arc 1 0.5 1,
>                                    straight $ pathFromVectors [(0,2)]]]
>
> t col = translateX (-2) $
>         unionA hcenter top [fillColor col $ roundRectF 1 5 0.1,
>                             translateY 1 . straight $ pathFromVectors [(3,0)]]
