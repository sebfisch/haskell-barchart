> import Graphics.Rendering.Diagrams
>
> main = renderAs PNG "logo.png" (Width 400) (pad 1 1 logo)
>
> logo = onBottom (yaxis:hspace 1:zipWith ($) [b,a,r,c,h,a,r,t] colors) // xaxis
>  where xaxis = line (30,0); yaxis = line (0,6)
>
> onBottom    = hcatA bottom
> colors      = cycle [seagreen,firebrick,midnightblue]
> line vec    = straight $ pathFromVectors [vec]
> bar col w h = fillColor col $ roundRectF w h 0.1
> hpull       = translateX . negate
> vpull       = translateY
>
> b col = onBottom [bar col 1 5, hpull 1 $ arc 1.5 0.6 0.4]
> a col = hpull 1 $ onBottom [hpull (-0.5) $ arc 1.2 0.1 0.9, bar col 1 2.7]
> r col = vpull 1 $ hcatA top [bar col 1 3, hpull 0.5 $ arc 2 0.5 0.75]
> c col = hpull 2 $ arc 2 0.1 0.9
> h col = hpull 1.4 $ onBottom [bar col 1 6, arcline]
>  where arcline = vcatA right [vpull 1 $ arc 1 0.5 1, line (0,2)]
> t col = hpull 2.5 $ unionA hcenter top [bar col 1 5, vpull 1 $ line (3,0)]
