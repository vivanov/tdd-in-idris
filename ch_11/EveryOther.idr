every_other : Stream a -> Stream a
every_other (x :: y :: zs) = y :: every_other zs
