open System.IO
open System


// === global size ===
let (gWidth, gHeight) = (320, 240)
let gSamples = 4
// === IO functions ===
let createImageCanvas (w, h) : int array = Array.zeroCreate (w * h * 3)
let writeImage (path, w, h, img) =
    let filestream = File.CreateText(path)
    filestream.WriteLine("P3")
    filestream.WriteLine(String.Format("{0} {1}", w, h))
    filestream.WriteLine("255")
    img |> Array.iter (fun (x: int) -> filestream.Write(String.Format("{0} ", x)))
    filestream.Close()
// test function
let testWhiteImage() = writeImage ("white.ppm", gWidth, gHeight, createImageCanvas (gWidth, gHeight) |> Array.map (fun x -> 255))
// testWhiteImage()

// === Basic Types ===
type Reflection = DIFF | SPEC | REFR
type Vec (x:float, y:float, z:float) = 
    member this.X = x
    member this.Y = y
    member this.Z = z

    static member inline Zero() = Vec(0.0, 0.0, 0.0)
    static member inline get_Zero() = Vec(0.0, 0.0, 0.0)
    static member inline ( + ) (v1: Vec, v2: Vec) = Vec(v1.X + v2.X, v1.Y + v2.Y, v1.Z + v1.Z)
    static member inline ( - ) (v1: Vec, v2: Vec) = Vec(v1.X - v2.X, v1.Y - v2.Y, v1.Z - v1.Z)
    static member inline ( * ) (v1: Vec, b: float) = Vec(v1.X * b, v1.Y * b, v1.Z * b)
    static member inline ( * ) (v1: Vec, v2: Vec) = Vec(v1.X * v2.X, v1.Y * v2.Y, v1.Z * v2.Z)
    member inline this.Norm() = this * (1.0 / (this.X * this.X + this.Y * this.Y + this.Z * this.Z))
    member inline this.Dot(b:Vec):float = this.X * b.X + this.Y * b.Y + this.Z * b.Z
    member inline this.Cross(b:Vec): Vec = Vec(this.Y*b.Z-this.Z*b.Y, this.Z*b.X-this.X*b.Z, this.X*b.Y-this.Y*b.X)

    override this.ToString() = "[" + this.X.ToString() + " " + this.Y.ToString() + " " + this.Z.ToString() + "]"

type Ray = {Origin: Vec; Direct: Vec}

type Sphere (radius: float, position: Vec, emission: Vec, color: Vec, reflect: Reflection) =
    member this.R = radius
    member this.P = position
    member this.E = emission
    member this.C = color
    member this.Reflect = reflect
    member this.Intersect (ray: Ray) : float =
        let op = this.P - ray.Origin
        let (t, eps) = (1e-4, 1e-4)
        let b = op.Dot(ray.Direct)
        let det = b * b - op.Dot(op) + this.R * this.R
        if det < 0.0 then infinity
        else
            let qdet = sqrt(det)
            match b-qdet>eps with
            | true  -> b-qdet
            | false ->
                match b+qdet>eps with
                | true -> b+qdet
                | false -> infinity
// === spheres ===
let spheres = [
    Sphere(1e5, Vec(  1.0e5+1.0,        40.8,         81.6), Vec.Zero(), Vec(0.75, 0.25, 0.25), DIFF); // left
    Sphere(1e5, Vec(-1.0e5+99.0,        40.8,         81.6), Vec.Zero(), Vec(0.25, 0.25, 0.75), DIFF); // right
    Sphere(1e5, Vec(       50.0,        40.8,        1.0e5), Vec.Zero(), Vec(0.75, 0.75, 0.75), DIFF); // back
    Sphere(1e5, Vec(       50.0,        40.8, -1.0e5+170.0), Vec.Zero(), Vec.Zero(),            DIFF); // front
    Sphere(1e5, Vec(       50.0,       1.0e5,         81.6), Vec.Zero(), Vec(0.75, 0.75, 0.75), DIFF); // bottom
    Sphere(1e5, Vec(       50.0, -1.0e5+81.6,         81.6), Vec.Zero(), Vec(0.75, 0.75, 0.75), DIFF); // top
    Sphere(16.5, Vec(27.0,       16.5, 47.0), Vec.Zero(), Vec(1.0, 1.0, 1.0) * 0.999, SPEC);   // mirror
    Sphere(16.5, Vec(73.0,       16.5, 78.0), Vec.Zero(), Vec(1.0, 1.0, 1.0) * 0.999, REFR);   // glas
    Sphere(600.0,Vec(50.0, 681.6-0.27, 81.6), Vec(12.0,12.0,12.0), Vec.Zero(),        DIFF);   // LITE
]
// === helper functions ===
let clamp (x:float):float = match x < 0.0 with | true -> 0.0 | false -> match x > 1.0 with | true -> 1.0 | false -> x
let toInt (x:float):int = int x
let intersect (ray:Ray) =
    let testResult = spheres |> List.map (fun x -> x.Intersect(ray))
    testResult |> List.mapi (fun i v -> i, v) |> List.minBy (fun (_,v) -> v)
// the ray tracer
let random = new Random()
let rec radiance (ray: Ray, old_depth: int) : Vec =
    let depth = old_depth + 1
    let (sphereId, t) = intersect(ray)
    match t=infinity with
    | true  -> Vec.Zero()
    | false ->
        let sph = spheres.[sphereId]
        let x = ray.Origin + ray.Direct * t
        let n = (x - sph.P).Norm()
        let nl = match n.Dot(ray.Direct) < 0.0 with | true -> n | false -> n * -1.0
        let p = max sph.C.X sph.C.Y |> max sph.C.Z

        if depth > 5 && random.NextDouble() >= p then
            // max depth and random failed
            sph.E
        else
            // continue to reflect
            let f = match depth > 5 with
                    | true  -> sph.C * (1.0 / p)
                    | false -> sph.C
            match sph.Reflect with
            | SPEC -> sph.E + f * radiance({Origin=x; Direct=ray.Direct-n*2.0*n.Dot(ray.Direct)}, depth)
            | DIFF -> // ideal diffuse
                let r1 = 2.0 * Math.PI * random.NextDouble()
                let r2 = random.NextDouble()
                let r2s = sqrt(r2)
                let w = nl
                let u = match Math.Abs(w.X) > 0.1 with
                        | true -> Vec(0.0, 1.0, 0.0).Cross(w).Norm()
                        | false-> Vec(1.0, 0.0, 0.0).Cross(w).Norm()
                let v = w.Cross(u)
                let d = (u * Math.Cos(r1)*r2s + v*Math.Sin(r1)*r2s + w*sqrt(1.0-r2)).Norm()
                sph.E + f * radiance({Origin=x; Direct=d}, depth)
            | REFR -> // ideal dielectric refraction
                let reflRay = {Origin=x; Direct=ray.Direct-n*2.0*n.Dot(ray.Direct)}
                let into = n.Dot(nl) > 0.0
                let nc = 1.0
                let nt = 1.5
                let nnt = match into with | true -> nc / nt | false -> nt / nc
                let ddn = ray.Direct.Dot(nl)
                let cos2t = 1.0-nnt*nnt*(1.0-ddn*ddn)
                if cos2t < 0.0 then
                    // total internal reflection
                    sph.E + f * radiance(reflRay, depth)
                else
                    let sign = match into with | true -> 1.0 | false -> -1.0
                    let tdir = (ray.Direct * nnt - n * (sign * (ddn*nnt+sqrt(cos2t)))).Norm()
                    let a = nt - nc
                    let b = nt + nc
                    let R0 = a*a/(b*b)
                    let c = 1.0 - (match into with | true -> -ddn | false -> tdir.Dot(n))
                    let Re = R0 + (1.0 - R0)*c*c*c*c*c
                    let Tr = 1.0 - Re
                    let P = 0.25+0.5*Re
                    let RP = Re / P
                    let TP = Tr / (1.0 - P)
                    // russian roulette
                    sph.E + f * (
                        match depth > 2 with
                        | true -> 
                            match random.NextDouble() < P with
                            | true -> radiance(reflRay, depth) * RP
                            | false-> radiance({Origin=x; Direct=tdir}, depth) * TP
                        | false ->
                            radiance(reflRay, depth) * Re + radiance({Origin=x; Direct=tdir}, depth) * Tr
                    )

// == rendering ==
let render() = 
    let cam = {Origin=Vec(50.0,52.0,295.6); Direct=Vec(0.0,-0.042612,-1.0).Norm()}
    let cx = Vec((float gWidth)*0.5135/(float gHeight), 0.0, 0.0)
    let cy = (cx.Cross(cam.Direct)).Norm()*0.5135
    let w = float gWidth
    let h = float gHeight

    let samplePixel x y =
        let r1 = 2.0 * random.NextDouble()
        let r2 = 2.0 * random.NextDouble()
        let sampleSubPixel sx sy dx dy =
            let d = cx *( ( (sx+0.5 + dx)/2.0 + x)/w - 0.5) + 
                    cy *( ( (sy+0.5 + dy)/2.0 + y)/h - 0.5) + cam.Direct
            radiance({Origin=cam.Origin+d*140.0; Direct=d.Norm()}, 0)

        let goThrough = [
            for sx in [0..1] do
                for sy in [0..1] do
                    for s in [0..gSamples-1] do
                        let dx = match r1 < 1.0 with | true -> sqrt(r1) - 1.0 | false -> 1.0 - sqrt(2.0-r1)
                        let dy = match r2 < 1.0 with | true -> sqrt(r2) - 1.0 | false -> 1.0 - sqrt(2.0-r2)
                        yield (sampleSubPixel (float sx) (float sy) dx dy)
            ]
        goThrough |> List.sum

    let canvas = createImageCanvas(gWidth, gHeight)
    
    for x in [0..gWidth-1] do
        for y in [0..gHeight-1] do
            printfn "pixel %i %i" x y
            let pixel = samplePixel (float x) (float y)
            let index = y * gWidth + x
            canvas.[index * 3 + 0] <- toInt pixel.X
            canvas.[index * 3 + 1] <- toInt pixel.Y
            canvas.[index * 3 + 2] <- toInt pixel.Z
            
    writeImage ("render.ppm", gWidth, gHeight, canvas)

render()